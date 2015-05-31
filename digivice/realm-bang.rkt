#!/usr/bin/env racket
#lang at-exp racket/gui

#|
Realm of Racket:    Learn to Program, one game at a time!
Author:             WarGrey Gyoudmon Ju
Maintainer:         <https://github.com/wargrey/discipline>
|#

@require{../digitama/digicore.rkt}

(require racket/date)

(define discipline-name "realm-of-racket")
(define realm-bang (dynamic-require (build-path (digimon-digitama) "engine" "legacy-world.rkt") 'big-bang void))

(date-display-format 'iso-8601)

(send* (make-object
           (class* frame% []
             (super-new [label "Realm of Racket"] [parent #false])
             
             (field [big-beam (make-object horizontal-panel% this)])
             
             (for ([rkt-path (in-list (find-digimon-files (curry regexp-match? @pregexp{.+\.rkt$})
                                                              (build-path (digimon-village) discipline-name)))])
               (dynamic-require rkt-path #false)
               (with-handlers ([exn? void])
                 (match-define {cons constructor settings} ((namespace-variable-value '~realm-bang #false void (module->namespace rkt-path))))
                 (define caption (string-join (string-split (path->string (last (explode-path rkt-path))) @pregexp{-|\.rkt$})))
                 (define label (hash-ref! settings '#:name caption))
                 (hash-remove! settings '#:island)
                 (hash-ref! settings '#:save-label? #false)
                 (define keyword-names (sort (hash-keys settings) keyword<?))
                 (make-object button% label big-beam
                   (lambda [<this> event]
                     (keyword-apply realm-bang
                                    keyword-names
                                    (map (curry hash-ref settings) keyword-names)
                                    constructor null
                                    #:island (string->symbol discipline-name))))))))
  (show #true) (center))
