#lang racket
(require setup/getinfo)
(require (submod "syntax/types.rkt" internals) (submod "interface.rkt" internals)
         (submod "syntax/rules.rkt" internals) (submod "syntax/syntax.rkt" internals))
(provide define-type define-representation declare-parametric-operator! define-constant define-operator define-ruleset load-herbie-plugins)

(define (module-exists? module)
  (with-handlers ([exn:fail:filesystem:missing-module? (const false)])
    (dynamic-require module #f)
    true))

(define (load-herbie-plugins)
  (for ([dir (find-relevant-directories '(herbie-plugin))])
    (define info
      (with-handlers ([exn:fail:filesystem? (const false)])
        (get-info/full dir)))
    (define value (info 'herbie-plugin (const false)))
    (when (and value (module-exists? value))
      (dynamic-require value #f))))