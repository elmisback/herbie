#lang racket

(require rival math/bigfloat)
(require "interface.rkt" "programs.rkt" "float.rkt")

(provide (struct-out symmetry-group) preprocess->sexp sexp->preprocess
         *herbie-preprocess* apply-preprocess ival-preprocesses
         remove-unnecessary-preprocessing)

;; Tracks list of preprocess structs Herbie decides to apply
(define *herbie-preprocess* (make-parameter empty))

;; Herbie preprocess structs
(struct symmetry-group (variables) #:prefab)


(define (preprocess->sexp preprocess)
  `(sort ,@(symmetry-group-variables preprocess)))

(define (sexp->preprocess sexp)
  (match sexp
    [(list 'sort vars ...) (symmetry-group vars)]
    [else (error (format "unknown preprocess ~a" sexp))]))

;; index-value-pairs is a sorted list of (index, value)
(define (list-set-multiple list index-value-pairs)
  (let loop ([current list] [todo index-value-pairs] [index 0])
    (cond
      [(empty? current)
       empty]
      [(and (not (empty? todo)) (equal? (first (first todo)) index))
       (cons (second (first todo)) (loop (rest current) (rest todo) (+ index 1)))]
      [else
       (cons (first current) (loop (rest current) todo (+ index 1)))])))

(define (<-repr repr a b)
  (< (repr->real a repr) (repr->real b repr)))

(define (apply-to-group variables point group-variables group-function)
  (define indicies
    (map (lambda (var) (index-of variables var)) group-variables))
  (define values
    (group-function (map (curry list-ref point) indicies)))
  (define sorted (sort (map list indicies values) (lambda (a b) (< (first a) (first b)))))
  (list-set-multiple point sorted))

(define (sort-group variables point preprocess repr)
  (apply-to-group variables point (symmetry-group-variables preprocess)
                  (lambda (group)
                    (sort group (curry <-repr repr)))))

(define (apply-preprocess variables sampled-point preprocess-structs repr)
  (cond
    [(empty? preprocess-structs)
     sampled-point]
    ;; Add more preprocess cases here- for now, only symmetry-group exists
    [else
     (apply-preprocess variables (sort-group variables sampled-point (first preprocess-structs) repr) (rest preprocess-structs) repr)]))


(define (ival-preprocess ivals precondition preprocess-struct)
  (apply-to-group (program-variables precondition) ivals (symmetry-group-variables preprocess-struct)
                  (lambda (group-ivals)
                    (ival-sort group-ivals bf<))))


(define (ival-preprocesses precondition preprocess-structs repr)
  (lambda ivals
    (let loop ([current ivals]
               [todo preprocess-structs])
      (cond
        [(empty? todo)
         (apply values current)]
        [else
         (loop (ival-preprocess current precondition (first todo))
               (rest todo))]))))

; until fixed point, iterate through preprocessing attempting to drop preprocessing with no effect on error
(define (remove-unecessary-preprocessing alt preprocessing #:removed [removed empty])
  (define-values (result newly-removed)
    (let loop ([preprocessing preprocessing] [i 0] [removed removed])
      (cond
        [(>= i (length preprocessing))
         (values preprocessing removed)]
        [(preprocessing-<=? alt (drop-at preprocessing i) preprocessing)
         (loop (drop-at preprocessing i) i (cons (list-ref preprocessing i) removed))]
        [else
         (loop preprocessing (+ i 1) removed)])))
  (cond
    [(< (length result) (length preprocessing))
     (remove-unecessary-preprocessing alt result #:removed newly-removed)]
    [else
     (timeline-push! 'remove-preprocessing (map (compose ~a preprocess->sexp) newly-removed))
     result]))
