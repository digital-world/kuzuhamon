#lang racket

(require "digitama/digicore.rkt")

(require scribble/core)
(require scribble/render)

(require (prefix-in markdown: scribble/markdown-render))

(module+ make:files
  (define /README.md (build-path (digimon-world) "README.md"))
  (define readme.scrbl (build-path (digimon-stone) "readme.scrbl"))
  
  (define mostly:readmes
    (list (list /README.md
                (cons readme.scrbl (filter-map (λ [dir] (and (get-info/full dir) (build-path dir "info.rkt")))
                                               (directory-list (digimon-world) #:build? #true)))
                (λ [target]
                  (parameterize ([current-directory (digimon-zone)]
                                 [exit-handler (thunk* (error 'make "[fatal] /~a needs a proper `exit-handler`!"
                                                              (find-relative-path (digimon-world) readme.scrbl)))])
                    (render (list (dynamic-require readme.scrbl 'doc)) (list (file-name-from-path target))
                            #:dest-dir (path-only target) #:render-mixin markdown:render-mixin #:quiet? #true))
                    (rename-file-or-directory (path-add-suffix target #".md") target #true)
                    (printf "  [Output to ~a]~n" target))))))
