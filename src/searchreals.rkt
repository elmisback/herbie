#lang racket
(require math/bigfloat rival)

(provide find-intervals)

(struct search-space (true false other))

(define (search-step ival-fn space midpoint split-var)
  (match-define (search-space true false other) space)
  (define-values (true* false* other*)
    (for/fold ([true* true] [false* false] [other* '()]) ([rect (in-list other)])
      (define res (apply ival-fn rect))
      (cond
       [(or (ival-err res) (not (ival-hi res)))
        (values true* (cons rect false*) other*)]
       [(and (not (ival-err? res)) (ival-lo res))
        (values (cons rect true*) false* other*)]
       [else
        (define range (list-ref rect split-var))
        (match (midpoint (ival-lo range) (ival-hi range))
          [(cons midleft midright)
           (define rect-lo (list-set rect split-var (ival (ival-lo range) midleft)))
           (define rect-hi (list-set rect split-var (ival midright (ival-hi range))))
           (values true* false* (list* rect-lo rect-hi other*))]
          [#f
           (values true* false* (cons rect other*))])])))
  (search-space true* false* other*))

(define (find-intervals ival-fn rects #:midpoint midpoint #:fuel [depth 128] #:log [log! void])
  (if (or (null? rects) (null? (first rects)))
      (map (curryr cons 'other) rects)
      (let loop ([space (search-space '() '() rects)] [n 0])
        (match-define (search-space true false other) space)
        (log! true false other)

        (define n* (remainder n (length (first rects))))
        (if (or (>= n depth) (empty? (search-space-other space)))
            (append (search-space-true space) (search-space-other space))
            (loop (search-step ival-fn space midpoint n*) (+ n 1))))))

