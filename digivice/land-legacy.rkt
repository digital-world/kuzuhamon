#!/usr/bin/env racket
#lang at-exp racket

#|
The Land of Lisp:   Learn to Program in Racket, One Game at a Time!
Author:             WarGrey Gyoudmon Ju
Maintainer:         <https://github.com/wargrey/discipline>
|#

@require{../digitama/digicore.rkt}

(require racket/date)

(define discipline-name "land-of-lisp")
(define land-repl (dynamic-require (build-path (digimon-digitama) "engine" "ancient-world.rkt") 'world-repl void))

(date-display-format 'iso-8601)

(printf "Welcome to the Land of Lisp~n")
(let land-bang ([text-games (filter list?
                                    (for/list ([rkt-path (find-digimon-files (curry regexp-match? @pregexp{.+\.rkt$})
                                                                             (build-path (digimon-village) discipline-name))])
                                      (dynamic-require rkt-path #false)
                                      (with-handlers ([exn? {λ _ #false}])
                                        (match-define {list {? symbol? grant} ..1} (namespace-variable-value '~grant #false void (module->namespace rkt-path)))
                                        (list (string-replace (path->string (file-name-from-path rkt-path)) @pregexp{-|\.rkt$} " ")
                                              grant
                                              (module->namespace rkt-path)))))])
  (for ([ginfo (in-list text-games)]
        [index (in-naturals 1)])
    (printf "[~a] ~a~n" index (car ginfo)))
  (printf "[0] <LEAVE THE LAND OF LISP>~n")
  (with-handlers ([exn:break? exit]
                  [exn? displayln])
    (define select (read))
    (when (or (eof-object? select) (= 0 select))
      (call-with-escape-continuation
       (compose1 (curryr raise #true)
                 (curry make-exn:break:terminate "land" (current-continuation-marks)))))
    (when (and (number? select) (<= select (length text-games)))
      (define ginfo (list-ref text-games (sub1 select)))
      (define-values {grant ns} (values (cadr ginfo) (caddr ginfo)))
      (call-with-input-string (format "~a~n" (first grant))
                              (curry land-repl grant ns))
      (land-repl grant ns)
      (land-bang text-games))))
