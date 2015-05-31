#lang at-exp racket/base

#|
Discipline:         Custom Ancient REPL [Class and Object Version]
Author:             WarGrey Gyoudmon Ju
Maintainer:         <https://github.com/wargrey/discipline>
|#

(provide (all-defined-out))

(require racket/match
         racket/bool
         racket/port
         racket/string
         racket/format
         racket/function
         racket/class)

(define ancient-world-x%
  (class object% (super-new)
    (abstract world-desc big-bang!)
    
    (define/public-final (world-read [in (current-input-port)])
      (define/match (wrap-read-line read-string)
        [{(? eof-object?)} "(:q)"]
        [{(pregexp @pregexp{^\s*$})} "(void)"]  ;; Do not delete this
        [{_} (string-append "(" read-string ")")])
      (call-with-input-string (wrap-read-line (read-line in)) {lambda [action-pool] (read action-pool)}))
    
    (define/public-final (world-print result #:print-void [print-void? #false])
      (define/match (world-format chars capital? literal?)
        [{(? null?) _ _} null]
        [{(list #\space tail ...) _ _} (cons #\space (world-format tail capital? literal?))]
        [{(list (or #\! #\? #\.) tail ...) _ _} (cons (car chars) (world-format tail #true literal?))]
        [{(list #\" tail ...) _ _} (world-format tail capital? (not literal?))]
        [{(list head tail ...) _ #true} (cons head (world-format tail #false #true))]
        [{(list #\# #\\ #\n #\e #\w #\l #\i #\n #\e tail ...) _ _} (cons #\newline (world-format tail capital? literal?))]
        [{(list head tail ...) #true _} (cons (char-upcase head) (world-format tail #false literal?))]
        [{(list head tail ...) _ _} (cons (char-downcase head) (world-format tail #false #false))])
      (unless (and (void? result) (false? print-void?))
        (printf "~a~n" (list->string (world-format (string->list (string-trim (format "~s" result) @pregexp{[()]})) #true #false))))
      (flush-output))
    
    (define/public-final (world-repl [in (current-input-port)] [on-alarm void] #:seconds [interval #f])
      (unless (exn:break? (with-handlers ([exn:break? identity]
                                          [exn? displayln])
                            (world-print (if (sync/timeout interval in)
                                               (let ([action (world-read in)])
                                                 (if (member (car action) '{exit quit :q})
                                                     (call-with-escape-continuation (compose1 (curryr raise #true)
                                                                                              (curry make-exn:break:terminate "world"
                                                                                                     (current-continuation-marks))))
                                                     (world-eval action)))
                                               (on-alarm)))))
        (if (send this world-over?)
            (world-print (send this last-words))
            (world-repl in on-alarm #:seconds interval))))
    
    (define/public (world-timeframe)
      (values #false void))
    
    (define/public (world-over?)
      #false)
    
    (define/public (last-words)
      (void))
    
    (define/private (world-eval command)
      (define-values {arguments action} (values (cdr command) (car command)))
      (define granted (remove* '{world-read world-eval world-repl world-over? last-words} (interface->method-names (class->interface this%))))
      (cond [(member action granted) (apply dynamic-send this action arguments)]
            [(symbol=? action 'void) (void)]
            [else '{"Sorry," your order is undefined.}]))))
