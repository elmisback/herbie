#lang racket

(require (only-in fpbench fpcore? supported-by-lang? core->js js-header) json file/gzip)
(require "../alternative.rkt" "../syntax/read.rkt" "../sandbox.rkt" "../interface.rkt")
(require "common.rkt" "timeline.rkt" "plot.rkt" "make-graph.rkt" "traceback.rkt" "../programs.rkt"
        "../syntax/sugar.rkt" "../float.rkt")
(provide all-pages make-page page-error-handler)

(define (unique-values pts idx)
  (length (remove-duplicates (map (curryr list-ref idx) pts))))

(define (all-pages result)
  (define test (test-result-test result))
  (define good? (test-success? result))
  (define others
    (if good?
      (let ([other (cdr (test-success-end-alts result))]
            [vars (test-vars test)])
        (if (and good? (< (* (length other) (length vars)) 100))
            (build-list (length other) (λ (x) (format "o~a" x)))
            '()))
      '()))

  (define pages
    `("graph.html"
      ,(and good? "interactive.js")
      "timeline.html" "timeline.json"
      ,@(for/list ([v (test-vars test)] [idx (in-naturals)]
                   #:when good? [type (append (list "" "r" "g" "b") others)]
                   #:unless (and (equal? type "g") (not (test-output test)))
                   ;; Don't generate a plot with only one X value else plotting throws an exception
                   #:when (> (unique-values (test-success-newpoints result) idx) 1))
          (format "plot-~a~a.png" idx type))
      ,(and good? (>= (length (test-success-end-alts result)) 2) "cost-accuracy.png")
      ,(and good? "points.json.gz")))
  (filter identity pages))

(define ((page-error-handler result page) e)
  (define test (test-result-test result))
  ((error-display-handler)
   (format "Error generating `~a` for \"~a\":\n~a\n" page (test-name test) (exn-message e))
   e))

(define (make-page page out result profile?)
  (define test (test-result-test result))
  (define repr (test-output-repr test))
  (match page
    ["graph.html"
     (match result
       [(? test-success?) (make-graph result out (get-interactive-js result repr) profile?)]
       [(? test-timeout?) (make-traceback result out profile?)]
       [(? test-failure?) (make-traceback result out profile?)])]
    ["interactive.js"
     (make-interactive-js result out repr)]
    ["timeline.html"
     (make-timeline (test-name test) (test-result-timeline result) out)]
    ["timeline.json"
     (write-json (test-result-timeline result) out)]
    ["cost-accuracy.png"
     (make-cost-accuracy-plot result out)]
    ["points.json.gz"
     (make-compressed-points result out repr)]
    [(regexp #rx"^plot-([0-9]+).png$" (list _ idx))
     (make-axis-plot result out (string->number idx))]
    [(regexp #rx"^plot-([0-9]+)([rbg]).png$" (list _ idx letter))
     (make-points-plot result out (string->number idx) (string->symbol letter))]
    [(regexp #rx"^plot-([0-9]+)(o[0-9]+).png$" (list _ idx letter))
     (make-points-plot result out (string->number idx) letter)]))

(define (get-interactive-js result repr)
  (define start-prog (alt-program (test-success-start-alt result)))
  (define end-prog (alt-program (car (test-success-end-alts result))))
  (define start-fpcore (program->fpcore (resugar-program start-prog repr)))
  (define end-fpcore (program->fpcore (resugar-program end-prog repr)))
  (and (fpcore? start-fpcore) (fpcore? end-fpcore)
       (supported-by-lang? start-fpcore "js")
       (supported-by-lang? end-fpcore "js")
       (string-append
          (js-header "Math")  ; pow, fmax, fmin will not work without this
          (core->js start-fpcore "start")
          (core->js end-fpcore "end"))))

(define (make-interactive-js result out repr)
  (define js-text (get-interactive-js result repr))
  (when (string? js-text)
    (display js-text out)))

; sends gzip-compressed bytes representing the points to out
(define (make-compressed-points result out repr)
  (define points (test-success-newpoints result))
  (define exacts (test-success-newexacts result))
  (define (json-dumps jsexpr) (with-output-to-string (lambda () (write-json jsexpr))))
  ;; borrowed from https://groups.google.com/g/racket-users/c/VZ0mxdkawao
  (define (compress str)
    (call-with-output-bytes
     (lambda (out-port)
       (call-with-input-string str
                              (lambda (in-port)
                                (gzip-through-ports in-port out-port #f 0))))))
  ;; can test decompress using the below
  ; (define (decompress str)
  ;   (call-with-output-string
  ;     (lambda (out-port)
  ;       (call-with-input-bytes str
  ;         (lambda (in-port)
  ;           (gunzip-through-ports in-port out-port))))))
  (define json-points (map (lambda (x) (value->json (car x) repr)) points))
  (define json-exacts (map (lambda (x) (value->json x repr)) exacts))
  (define json-obj `#hasheq((points . ,json-points) (exacts . ,json-exacts)))
  (display (compress (json-dumps json-obj)) out))
