#lang typed/racket

(require "../../DigiGnome/digitama/digicore.rkt")

(provide (all-defined-out))
(provide (all-from-out "../../DigiGnome/digitama/digicore.rkt"))

(current-digimon "Kuzuhamon")

(define digimon-island : (Parameterof Path-String) (make-parameter (build-path (digimon-terminus) "island")))
