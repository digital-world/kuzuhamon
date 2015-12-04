#lang typed/racket

(provide (all-defined-out))
(provide (all-from-out "../../DigiGnome/digitama/digicore.rkt"))
(provide (all-from-out "../../DigiGnome/digitama/emoji.rkt"))

(require "../../DigiGnome/digitama/digicore.rkt")
(require "../../DigiGnome/digitama/emoji.rkt")

(current-digimon "Kuzuhamon")

(define discipline-name : String (path->string (find-system-path 'run-file)))
(define digimon-island : (Parameterof Path-String) (make-parameter (build-path (digimon-terminus) "island")))
