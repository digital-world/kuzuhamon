#!/bin/sh
#|
exec racket --name "land-of-lisp" --require "$0" -- ${1+"$@"}
|#

#lang at-exp racket

#|
The Land of Lisp:   Learn to Program in Racket, One Game at a Time!
Author:             WarGrey Gyoudmon Ju
Maintainer:         <https://github.com/wargrey/discipline>
|#

@require{../digitama/digicore.rkt}
@require{../digitama/engine/ancient-world-x.rkt}

(printf "Welcome to the Land of Lisp~n")
(let land-bang ([text-games (filter-map {lambda [rkt-path]
                                          (dynamic-require rkt-path #false)
                                          (define ns (module->namespace rkt-path))
                                          (define maybe-mains (filter {lambda [<v>] (subclass? <v> ancient-world-x%)}
                                                                      (map {lambda [<v>] (namespace-variable-value <v> #false void ns)}
                                                                           (namespace-mapped-symbols ns))))
                                          (cond [(null? maybe-mains) #false]
                                                [else (new (car maybe-mains))])}
                                        (find-digimon-files (curry regexp-match? @pregexp{.+\.rkt$})
                                                            (build-path (digimon-village) discipline-name)))])
  (for ([game (in-list text-games)]
        [index (in-naturals 1)])
    (printf "[~a] ~a~n" index (send game world-desc)))
  (printf "[0] <LEAVE THE LAND OF LISP>~n")
  (with-handlers ([exn:break? exit]
                  [exn? displayln])
    (define select (read))
    (when (or (eof-object? select) (= 0 select))
      (call-with-escape-continuation
       (compose1 (curryr raise #true)
                 (curry make-exn:break:terminate "land" (current-continuation-marks)))))
    (when (and (number? select) (<= select (length text-games)))
      (define game (list-ref text-games (sub1 select)))
      (define-values {interval on-alarm} (send game world-timeframe))
      (send game world-print (send game big-bang!))
      (send game world-repl (current-input-port) on-alarm #:seconds interval))
    (newline)
    (land-bang text-games)))
