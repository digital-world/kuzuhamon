#lang at-exp racket

(define ~realm-bang
  {lambda [] #| lazy reference |#
    (cons {lambda [] (interval% 0 100)}
          (make-hash `{{#:on-key . ,deal-operation!}
                       {#:to-draw . ,render!}
                       {#:stop-when . ,bingo!?}
                       {#:record? . guessing-gooey}
                       {#:keyframe? . ,(const #true)}
                       {#:tick-rate . 8}
                       {#:name . "Guess My Number"}}))})


#|
Realm of Racket:    big-bang: Guessing Gooey
Synopsis:           GUI Version of the Guess My Number Game.
Description:        ↑ for larger numbers, ↓ for smaller ones, = means that's it. 
Author:             WarGrey Gyoudmon Ju
Maintainer:         <https://github.com/wargrey>
|#

(require racket/draw)
(require pict)

@require{../../digitama/engine/legacy-world.rkt}

(struct interval% {lower upper} #:transparent)

(define-values [TEXT-COLOR TEXT-SIZE] (values (list (send the-color-database find-color "blue")) 11))
(define-values [HELP-OPERATION HELP-TERMINATION] (values (text "↑ for larger numbers, ↓ for smaller ones" TEXT-COLOR TEXT-SIZE)
                                                         (text "press = when your number is guessed, q to quit" TEXT-COLOR TEXT-SIZE))) 
(define-values [WIDTH HEIGHT SIZE COLOR] (values (+ (floor (pict-width HELP-TERMINATION)) 10) 150 72 (list (send the-color-database find-color "red"))))
(define-values [BG-SCENE] (values (ct-superimpose (cb-superimpose (colorize (filled-rectangle WIDTH HEIGHT) "white") HELP-TERMINATION) HELP-OPERATION)))

;; guesse :: interval% -> Number
(define (guess world)
  (quotient (+ (interval%-lower world) (interval%-upper world)) 2))

;; smaller :: interval% -> interval%
(define (smaller world)
  (interval% (interval%-lower world) (max (interval%-lower world) (sub1 (guess world)))))

;; bigger :: interval% -> interval%
(define (bigger world)
  (interval% (min (interval%-upper world) (add1 (guess world))) (interval%-upper world)))

;; deal-operation! :: interval% -> symbol? -> interval%
;; Handles key events
(define (deal-operation! world keyname)
  (cond [(equal? keyname 'up) (bigger world)]
        [(equal? keyname 'down) (smaller world)]
        [(equal? keyname #\=) (stop-with world)]
        [(equal? keyname #\q) (stop-with world)]
        [else world]))

;; render :: interval% -> IO ()
;; Handles draw event
(define (render! world)
  (cc-superimpose BG-SCENE (text (if (bingo!? world) (symbol->string 'END) (number->string (guess world))) COLOR SIZE)))

;; bingo!? :: interval% -> Bool
;; Tell big-bang when to stop the game
(define (bingo!? world)
  (= (interval%-lower world) (interval%-upper world)))
