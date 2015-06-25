#lang at-exp typed/racket

#|
The Nature of Code: Introduction: The Random Utilities
Author:             WarGrey Gyoudmon Ju
Maintainer:         <https://github.com/wargrey/discipline>
|#

(provide (all-defined-out)
         (all-from-out "perlin-noise.rkt"))

(require racket/fixnum)

(require math/flonum)
(require math/distributions)

@require{perlin-noise.rkt}

(define random-pseudo : (case-> [-> Flonum]
                                [Zero -> Flonum]
                                [Positive-Fixnum -> Nonnegative-Fixnum]
                                [Negative-Fixnum -> Fixnum]
                                [Inexact-Real-Nan -> Inexact-Real-Nan])
  (case-lambda [{} (random)]
               [{range} (cond [(zero? range) (fl- (fl* (random) 2.0) 1.0)]
                              [(exact-positive-integer? range) (random range)]
                              [(exact-nonnegative-integer? range) (fx+ (fx* (random (cast (abs range) Positive-Integer)) 2) range)]
                              [else #| NaN.0 |# range])]))

(define random-gaussian : (case-> [-> Flonum]
                                  [Flonum -> Flonum]
                                  [Flonum Flonum -> Flonum])
  (lambda {[μ 0.0] [σ 1.0]}
    (flvector-ref (flnormal-sample μ σ 1) 0)))

(define random-montecarlo : (case-> [-> Flonum]
                                    [(Flonum -> Flonum) -> Flonum])
  (lambda {[formula identity]}
    (define-values {#{probability : Flonum} #{qualifying : Flonum}} (values (random) (random)))
    (cond [(qualifying . < . (formula probability)) probability]
          [else (random-montecarlo formula)])))
