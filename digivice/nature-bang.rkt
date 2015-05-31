#!/bin/sh
#|
exec racket --name "the-nature-of-code" --require "$0" -- ${1+"$@"}
|#

#lang at-exp typed/racket

#|
The Nature of Code: Simulating natural system with Racket
Author:             WarGrey Gyoudmon Ju
Maintainer:         <https://github.com/wargrey/discipline>
|#

@require{../digitama/engine/digital-world.rkt}

(require typed/mred/mred)

(define discipline-name : String (path->string (find-system-path 'run-file)))

(define nature-bang% 
  (class frame%
    (override on-superwindow-show)
    (inherit show center)
    
    (super-new [label "Loading ..."] [parent #false])
    
    (define frame-bang : (Instance Horizontal-Panel%) (instantiate horizontal-panel% {this}))
    
    (define (on-superwindow-show shown?)
      (when shown?
        (define total (find-digimon-files (curry regexp-match? @pregexp{(?<!random-walks)\.rkt$})
                                          (build-path (digimon-village) discipline-name)))
        (define p% (with-handlers ([exn? {λ _ 1.0}]) (/ 100 (length total))))
        (for ([rkt-module (in-list total)]
              [progress (in-naturals)])
          (dynamic-require rkt-module #false)
          (define-values {file-name namespace} (values (path->string (cast (file-name-from-path rkt-module) Path)) (module->namespace rkt-module)))
          (define ~main (namespace-variable-value '~nature-bang #false void namespace))
          (when (regexp? ~main)
            (define small-bang (new group-box-panel% [label (string-join (string-split file-name @pregexp{-|\.rkt$}))] [parent frame-bang] [alignment '{left top}]))
            (define bonus-bang (new group-box-panel% [label "Bonus"] [parent small-bang] [style '{deleted}] [alignment '{left top}]))
            (define save-dirname (string-replace file-name @pregexp{\.rkt$} ""))
            (send this set-label (format "Loading ~a (~a)" save-dirname (exact-floor (* progress p%))))
            (for ([bang (in-list (namespace-mapped-symbols namespace))]
                  #:when (regexp-match? ~main (symbol->string bang)))
              (define ~nature-bang (namespace-variable-value bang #false void namespace))
              (when (and (procedure? ~nature-bang) (equal? 0 (procedure-arity ~nature-bang)))
                (define ~world.settings ((cast ~nature-bang (-> Any))))
                (when (pair? ~world.settings)
                  (define world (car ~world.settings))
                  (define keywords (cdr ~world.settings))
                  (when (hash? keywords)
                    (define settings (cast keywords (HashTable Big-Bang-Keyword Any)))
                    (define caption (string-join (string-split (symbol->string bang) @pregexp{-|%$})))
                    (define bonus-caption (string-replace caption @pregexp{Bonus:} ""))
                    ((inst hash-ref! Big-Bang-Keyword Any) settings '#:name {λ _ bonus-caption})
                    ((inst hash-set! Big-Bang-Keyword Any) settings '#:island (string->symbol discipline-name))
                    ((inst hash-ref! Big-Bang-Keyword Any) settings '#:record? {λ _ (string->symbol save-dirname)})
                    ((inst hash-ref! Big-Bang-Keyword Any) settings '#:save-label? {λ _ #true})
                    (define keyword-eval ((inst append Any) `(big-bang ,world)
                                                            (let kw-apply : (Listof Any) ([keys ((inst hash-keys Big-Bang-Keyword Any) settings)])
                                                              (cond [(null? keys) '()]
                                                                    [else (define k (car keys))
                                                                          (define v (hash-ref settings k))
                                                                          (append `(,k ,(if (symbol? v) `',v v))
                                                                                  (kw-apply (cdr keys)))]))))
                    (make-object button%
                      (cast (hash-ref settings '#:name) String)
                      (if (string=? caption bonus-caption) small-bang bonus-bang)
                      {λ [<this> event] (eval keyword-eval namespace) (void)})))))
            (unless (null? (send bonus-bang get-children))
              (send small-bang add-child bonus-bang))
            (send this center 'both)))
        (send this set-label "The Nature of Code")))))

(send* (make-object nature-bang%) (show #true) (center 'both))
