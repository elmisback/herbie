#lang racket
(require (only-in xml write-xexpr xexpr?) 
         (only-in fpbench fpcore? supported-by-lang?
                          core->c core->tex expr->tex
                          [core-common-subexpr-elim core-cse]
                          *expr-cse-able?*))

(require "../common.rkt" "../syntax/read.rkt" "../programs.rkt"
         "../interface.rkt" "../preprocess.rkt" "../syntax/sugar.rkt")

(provide render-menu render-warnings render-large render-program program->fpcore render-reproduction js-tex-include)

(define (program->fpcore prog #:ident [ident #f])
  (match-define (list _ args expr) prog)
  (if ident
      (list 'FPCore ident args expr)
      (list 'FPCore args expr)))

(define (fpcore-add-props core props)
  (match core
   [(list 'FPCore name args expr) `(FPCore ,name ,args ,@props ,expr)]
   [(list 'FPCore args expr) `(FPCore ,args ,@props ,expr)]))

(define (at-least-two-ops? expr)
  (match expr
   [(list op args ...) (ormap list? args)]
   [_ #f]))

(define (fpcore->string core)
  (define-values (ident args props expr)
    (match core
     [(list 'FPCore name (list args ...) props ... expr) (values name args props expr)]
     [(list 'FPCore (list args ...) props ... expr) (values #f args props expr)]))
  (define props*  ; make sure each property (name, value) gets put on the same line
    (for/list ([(prop name) (in-dict (apply dict-set* '() props))]) ; how to make a list of pairs from a list
      (format "~a ~a" prop name)))
  (define top (if ident (format "FPCore ~a ~a" ident args) (format "FPCore ~a" args)))
  (pretty-format `(,top ,@props* ,expr) #:mode 'display))

(define/contract (render-menu sections links)
  (-> (listof (or/c (cons/c string? string?) #f)) (listof (cons/c string? string?)) xexpr?)
  `(nav ([id "links"])
    (div
     ,@(for/list ([(text url) (in-dict (filter identity links))])
         `(a ([href ,url]) ,text)))
    (div
     ,@(for/list ([(text url) (in-dict (filter identity sections))])
         `(a ([href ,url]) ,text)))))

(define/contract (render-warnings warnings)
  (-> (listof (list/c symbol? string? (listof any/c) (or/c string? #f) (listof string?))) xexpr?)
  (if (null? warnings)
      ""
      `(ul ([class "warnings"])
           ,@(for/list ([warning warnings])
               (match-define (list type message args url extra) warning)
               `(li (h2 ,(apply format message args)
                        ,(if url `(a ([href ,url]) " (more)") ""))
                    ,(if (null? extra)
                         ""
                         `(ol ([class "extra"])
                              ,@(for/list ([line extra])
                                  `(li ,line)))))))))

(define (render-large #:title [title #f] name . values)
  `(div ,name ": " (span ([class "number"]
                          ,@(if title `([title ,title]) '()))
                         ,@values)))
  
(define languages
  `(("TeX" . ,(λ (c i) (core->tex c)))
    ("FPCore" . ,(λ (c i) (fpcore->string c)))
    ("C" . ,(λ (c i) (core->c c (if i (symbol->string i) "code"))))))

(define (render-preprocess-struct preprocess)
  (define vars (string-append "[" (string-join (map symbol->string (symmetry-group-variables preprocess)) ", ") "]"))
  `(div ([class "program math"])
        "\\[" ,vars "=" ,(string-append "\\mathsf{sort}(" vars ")") "\\]"))

(define (render-preprocess preprocess-structs)
  `(div ([id "preprocess"])
        ,@(map render-preprocess-struct preprocess-structs)))

(define (render-program #:to [result #f] preprocess test)
  (define identifier (test-identifier test))
  (define output-repr (test-output-repr test))

  (define in-prog (program->fpcore (resugar-program (test-program test) output-repr) #:ident identifier))
  (define out-prog
    (and result
         (parameterize ([*expr-cse-able?* at-least-two-ops?])
           (core-cse (program->fpcore (resugar-program result output-repr) #:ident identifier)))))

  (define output-prec (representation-name output-repr))
  (define in-prog* (fpcore-add-props in-prog (list ':precision output-prec)))
  (define out-prog* (and out-prog (fpcore-add-props out-prog (list ':precision output-prec))))

  (define versions
    (reap [sow]
      (for ([(lang converter) (in-dict languages)])
        (let ([ext (string-downcase lang)]) ; FPBench organizes compilers by extension
          (when (and (fpcore? in-prog*) (or (not out-prog*) (fpcore? out-prog*))
                     (or (equal? ext "fpcore")                           
                          (and (supported-by-lang? in-prog* ext) ; must be valid in a given language  
                               (or (not out-prog*) (supported-by-lang? out-prog* ext)))))
            (sow (cons lang (cons (converter in-prog* identifier)
                                  (and out-prog* (converter out-prog* identifier)))))
    )))))

  (define-values (math-in math-out)
    (if (dict-has-key? versions "TeX")
        (let ([val (dict-ref versions "TeX")])
          (values (car val) (cdr val)))
        (values "" "")))

  `(section ([id "program"])
     ,(if (equal? (program-body (test-precondition test)) '(TRUE))
          ""
          `(div ([id "precondition"])
             (div ([class "program math"])
                  "\\[" ,(expr->tex (resugar-program (program-body (test-precondition test)) output-repr)) "\\]")))
     ,(if (empty? preprocess)
          ""
          (render-preprocess preprocess))
           
     (select ([id "language"])
       (option "Math")
       ,@(for/list ([lang (in-dict-keys versions)])
           `(option ,lang)))
     (div ([class "implementation"] [data-language "Math"])
       (div ([class "program math"]) "\\[" ,math-in "\\]")
       ,@(if result
             `((div ([class "arrow"]) "↓")
               (div ([class "program math"]) "\\[" ,math-out "\\]"))
             `()))
     ,@(for/list ([(lang outs) (in-dict versions)])
         (match-define (cons out-input out-output) outs)
         `(div ([class "implementation"] [data-language ,lang])
            (pre ([class "program"]) ,out-input) 
            ,@(if out-output   
                  `((div ([class "arrow"]) "↓")
                    (pre ([class "program"]) ,out-output))  
                  `())))))

(define/contract (render-command-line)
  (-> string?)
  (format
   "herbie shell --seed ~a ~a"
   (if (vector? (get-seed)) (format "'~a'" (get-seed)) (get-seed))
   (string-join
    (for/list ([rec (changed-flags)])
      (match rec
        [(list 'enabled class flag) (format "+o ~a:~a" class flag)]
        [(list 'disabled class flag) (format "-o ~a:~a" class flag)]))
    " ")))

(define/contract (render-fpcore test)
  (-> test? string?)
  (define output-repr (test-output-repr test))
  (string-join
   (filter
    identity
    (list
     (if (test-identifier test)
         (format "(FPCore ~a ~a" (test-identifier test) (test-vars test))
         (format "(FPCore ~a" (test-vars test)))
     (format "  :name ~s" (test-name test))
     (format "  :precision ~s" (representation-name (test-output-repr test)))
     (if (equal? (program-body (test-precondition test)) '(TRUE))
         #f
         (format "  :pre ~a" (resugar-program (program-body (test-precondition test)) output-repr)))
     (if (equal? (test-expected test) #t)
         #f
         (format "  :herbie-expected ~a" (test-expected test)))
     (if (test-output test)
         ;; Extra newlines for clarity
         (format "\n  :herbie-target\n  ~a\n" (resugar-program (test-output test) output-repr))
         #f)
     (format "  ~a)" (resugar-program (test-input test) output-repr))))
   "\n"))

(define/contract (render-reproduction test #:bug? [bug? #f])
  (->* (test?) (#:bug? boolean?) xexpr?)

  `(section ((id "reproduce"))
    (h1 "Reproduce")
    ,(if bug?
         `(p "Please include this information when filing a "
             (a ((href "https://github.com/herbie-fp/herbie/issues")) "bug report") ":")
         "")
    (pre ((class "shell"))
         (code
          ,(render-command-line) "\n"
          ,(render-fpcore test) "\n"))))

(define js-tex-include
  '((link ([rel "stylesheet"] [href "https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.css"]
           [integrity "sha384-9tPv11A+glH/on/wEu99NVwDPwkMQESOocs/ZGXPoIiLE8MU/qkqUcZ3zzL+6DuH"]
           [crossorigin "anonymous"]))
    (script ([src "https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.js"]
             [integrity "sha384-U8Vrjwb8fuHMt6ewaCy8uqeUXv4oitYACKdB0VziCerzt011iQ/0TqlSlv8MReCm"]
             [crossorigin "anonymous"]))
    (script ([src "https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/contrib/auto-render.min.js"]
             [integrity "sha384-aGfk5kvhIq5x1x5YdvCp4upKZYnA8ckafviDpmWEKp4afOZEqOli7gqSnh8I6enH"]
             [crossorigin "anonymous"]))))
             
