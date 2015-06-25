#lang at-exp racket/base

(define ~grant '{start-over! bigger! smaller!})

;;; land-of-lisp: The Guessing Game
;;; In this game, you pick a number from 1 to 100,
;;;   then the computer guess it with tips `bigger` and `smaller`."

@require{../../digitama/engine/ancient-world.rkt}

(define-values {small big} (values #f #f))

(define {guess}
  (arithmetic-shift (+ small big) -1))

(define (start-over!)
  (set!-values {small big} (values 1 100))
  (world-print '{Mission Start! (Thinking a number between 1 and 100 in your mind.)})
  (guess))

(define (bigger!)
  (set! small (+ (guess) 1))
  (guess))

(define {smaller!}
  (set! big (- (guess) 1))
  (guess))
