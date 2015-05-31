#lang at-exp racket/base

#|
Discipline:         Custom Ancient REPL
Author:             WarGrey Gyoudmon Ju
Maintainer:         <https://github.com/wargrey/discipline>
|#

(provide world-read
         world-eval
         world-print
         world-repl)

(require racket/match
         racket/bool
         racket/port
         racket/string
         racket/format
         racket/function)

(define (world-read [in (current-input-port)])
  (define/match (wrap-read-line read-string)
    [{(? eof-object?)} "(quit)"]
    [{(pregexp @pregexp{^\s*$})} "(void)"]
    [{_} (string-append "(" read-string ")")])
  (call-with-input-string (wrap-read-line (read-line in))
                          {lambda [action-pool]
                            (define action (read action-pool))
                            (cons (car action) (map {lambda [args] (list 'quote args)} (cdr action)))}))

(define (world-eval command grant ns)
  (define-values {argument action} (values (cdr command) (car command)))
  (define action! (string->symbol (string-append (symbol->string action) "!")))
  (cond [(member action grant) (eval (cons action argument) ns)]
        [(member action! grant) (eval (cons action! argument) ns)]
        [(symbol=? action 'void) (void)]
        [else '{"Sorry," your order is undefined.}]))

(define (world-print result)
  (define/match (world-format chars capital? literal?)
    [{(? null?) _ _} null]
    [{(list #\space tail ...) _ _} (cons #\space (world-format tail capital? literal?))]
    [{(list (or #\! #\? #\.) tail ...) _ _} (cons (car chars) (world-format tail #true literal?))]
    [{(list #\" tail ...) _ _} (world-format tail capital? (not literal?))]
    [{(list head tail ...) _ #true} (cons head (world-format tail #false #true))]
    [{(list head tail ...) #true _} (cons (char-upcase head) (world-format tail #false literal?))]
    [{(list head tail ...) _ _} (cons (char-downcase head) (world-format tail #false #false))])
  (printf "~a~n" (list->string (world-format (string->list (string-trim (format "~s" result) @pregexp{[()]})) #true #false)))
  (flush-output))

(define (world-repl grant ns [in (current-input-port)])
  (unless (exn:break? (with-handlers ([exn:break? identity]
                                      [exn? displayln])
                        (define action (world-read in))
                        (cond [(member (car action) '{exit quit :q}) (call-with-escape-continuation
                                                                      (compose1 (curryr raise #true)
                                                                                (curry make-exn:break:terminate "world" (current-continuation-marks))))]
                              [else (define result (world-eval action grant ns))
                                    (unless (void? result)
                                      (world-print result))])))
    (world-repl grant ns in)))
