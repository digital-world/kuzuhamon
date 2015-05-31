#!/usr/bin/env racket
#lang at-exp racket/gui

#|
The Nature of Code: Simulating natural system with Racket
Author:             WarGrey Gyoudmon Ju
Maintainer:         <https://github.com/wargrey/discipline>
|#

@require{../digitama/digicore.rkt}

(require racket/date)

(define discipline-name "the-nature-of-code")
(define nature-bang (dynamic-require (build-path (digimon-digitama) "engine" "legacy-world.rkt") 'big-bang void))

(date-display-format 'iso-8601)

(send* (make-object
           {class* frame% []
             (super-new [label "The Nature of Code"] [parent #false])
             
             (field [big-bang (instantiate horizontal-panel% {this})]
                    [launchers (filter-map {lambda [rkt-path]
                                             (dynamic-require rkt-path #false)
                                             (define-values {file-name namespace} (values (path->string (last (explode-path rkt-path))) (module->namespace rkt-path)))
                                             (match (namespace-variable-value '~nature-bang #false void namespace)
                                               [(? regexp? ~main) (define small-bang (new group-box-panel% [label (string-join (string-split file-name @pregexp{-|\.rkt$}))] [parent big-bang] [alignment '{left top}]))
                                                                  (define bonus-bang (new group-box-panel% [label "Bonus"] [parent small-bang] [style '{deleted}] [alignment '{left top}]))
                                                                  (filter-map {lambda [bang]
                                                                                (match (with-handlers ([exn? void]) ((namespace-variable-value bang #false void namespace)))
                                                                                  [(cons (? procedure? constructor) (? hash? settings))(define caption (string-join (string-split (symbol->string bang) @pregexp{-|%$})))
                                                                                                                                       (define bonus-caption (string-replace caption @pregexp{Bonus:} ""))
                                                                                                                                       (define label (hash-ref! settings '#:name bonus-caption))
                                                                                                                                       (hash-set! settings '#:island (string->symbol discipline-name))
                                                                                                                                       (unless (zero? (procedure-arity constructor))
                                                                                                                                         (hash-ref! settings '#:width 640)
                                                                                                                                         (hash-ref! settings '#:height 600))
                                                                                                                                       (hash-ref! settings '#:record? (string-replace file-name @pregexp{\.rkt$} ""))
                                                                                                                                       (hash-ref! settings '#:save-label? #true)
                                                                                                                                       (define keyword-names (sort (hash-keys settings) keyword<?))
                                                                                                                                       (make-object button% label (if (string=? caption bonus-caption) small-bang bonus-bang)
                                                                                                                                         {lambda [<this> event]
                                                                                                                                           (keyword-apply nature-bang
                                                                                                                                                          keyword-names (map (curry hash-ref settings) keyword-names)
                                                                                                                                                          constructor null)})]
                                                                                  [_ #false])}
                                                                              (filter (compose1 (curry regexp-match? ~main) symbol->string)
                                                                                      (namespace-mapped-symbols namespace)))
                                                                  (unless (null? (send bonus-bang get-children))
                                                                    (send small-bang add-child bonus-bang))]
                                               [_ #false])}
                                           (find-digimon-files (curry regexp-match? @pregexp{random-walks\.rkt$})
                                                               (build-path (digimon-village) discipline-name)))])})
  (show #true) (center))
