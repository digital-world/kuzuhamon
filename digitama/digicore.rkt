#lang typed/racket

(provide (all-defined-out))
(provide (all-from-out "../../wisemon/digitama/digicore.rkt"))
(provide (all-from-out "../../wisemon/digitama/emoji.rkt"))

(require "../../wisemon/digitama/digicore.rkt")
(require "../../wisemon/digitama/emoji.rkt")

(current-digimon "kuzuhamon")

(define discipline-name : String (path->string (find-system-path 'run-file)))
(define digimon-island : (Parameterof Path-String) (make-parameter (build-path (digimon-terminus) "island")))
