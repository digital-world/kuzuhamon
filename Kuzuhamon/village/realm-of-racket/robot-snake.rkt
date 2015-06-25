#lang at-exp racket

(define ~realm-bang
  {lambda [] #| lazy reference |#
    (cons {lambda [] (pit% (snake% 'right (position% 1 1) null) (build-list MAX-GOO {lambda [who-cares] (birth-goo)}))}
          (make-hash `{{#:to-draw . ,render!}
                       {#:on-tick . ,move}
                       {#:tick-rate . ,TICK-RATE}
                       {#:on-key . ,direct-snake!}
                       {#:stop-when . ,dead?}
                       {#:last-draw . ,render-when-game-over!}
                       {#:name . "The Robot Snake"}}))})

#|
Realm of Racket:    Recursion is easy: robot snake
Synopsis:           The Robot Snake Game
Description:        The player uses the arrow keys to control a snake, which is placed in a rectangular pit.
                    Appearing and disappearing goo items are scattered all around the pit, and it’s up o the
                    snake to devour them. When the snake eats a piece of goo, though, a new one appears at a
                    random position in the pit. In order for the snake to eat, its head must be over a piece of goo.
Author:             WarGrey Gyoudmon Ju
Maintainer:         <https://github.com/wargrey>
|#

(require pict)
(require racket/draw)

@require{../../digitama/engine/legacy-world.rkt}

(define snake-nature (build-path (digimon-terminus) "island" "realm-of-racket" "nature" "robot-snake"))

;;; position% :: (Integer, Integer)
;;; snake% :: (Symbol = {'up | 'down | 'left | 'right}, position%, [position%])
;;; goo% :: (position%, Integer)
;;; pit% :: (snake%, goo%)
(struct position% {x y} #:transparent)
(struct snake% {direction head body} #:transparent)
(struct goo% {location expire} #:transparent)
(struct pit% {snake goos} #:transparent)

(define-values [TICK-RATE BOARD-LENGTH GRID-SIZE FONT-SIZE MAX-GOO EXPIRATION-TIME] (values 10 30 15 15 4 128))
(define-values [WIDTH-PX HEIGHT-PX] (values (* GRID-SIZE 30) (* GRID-SIZE 30)))
(define-values [BG-SCENE GOO-IMG HEAD-IMG BODY-IMG TAIL-IMG] (values (colorize (filled-rectangle WIDTH-PX HEIGHT-PX) "white")
                                                                     (bitmap (build-path snake-nature "goo.gif"))
                                                                     (bitmap (build-path snake-nature "head.gif"))
                                                                     (bitmap (build-path snake-nature "body.gif"))
                                                                     (bitmap (build-path snake-nature "tail.gif"))))
(define HEAD-IMGS `[[left ,HEAD-IMG]
                    [down ,(rotate HEAD-IMG (/ pi 2))]
                    [right ,(rotate HEAD-IMG pi)]
                    [up ,(rotate HEAD-IMG (* pi 3/2))]])

;; birth-goo :: -> goo%
;; Give birth to a new goo
(define (birth-goo)
  (goo% (position% (add1 (random (sub1 BOARD-LENGTH))) (add1 (random (sub1 BOARD-LENGTH)))) EXPIRATION-TIME))

;; deal-operation! :: pit% -> key-event -> pit%
;; Handles key events
(define (direct-snake! world keyname)
  (define (opposite-direct? current target)
    (cond [(symbol=? current 'up) (symbol=? target 'down)]
          [(symbol=? current 'down) (symbol=? target 'up)]
          [(symbol=? current 'left) (symbol=? target 'right)]
          [(symbol=? current 'right) (symbol=? target 'left)]))
  (define (direct snake direction)
    (cond [(and (opposite-direct? (snake%-direction snake) direction) (cons? (snake%-body snake))) (stop-with world)]
          [else (pit% (snake% direction (snake%-head snake) (snake%-body snake)) (pit%-goos world))]))
  (case keyname
    [{up down left right} (direct (pit%-snake world) keyname)]
    [else world]))

;; move :: pit% -> pit%
;; Handles enter frame event
(define (move world)
  (define (colliding? snake goo)
    (define p1 (snake%-head snake))
    (define p2 (goo%-location goo))
    (and (= (position%-x p1) (position%-x p2))
         (= (position%-y p1) (position%-y p2))))
  (define (encounter-goo snake goos)
    (cond [(empty? goos) #false]
          [(colliding? snake (car goos)) (car goos)]
          [else (encounter-goo snake (cdr goos))]))
  (define (time-flies goo)
    (cond [(zero? (goo%-expire goo)) (birth-goo)]
          [else (goo% (goo%-location goo) (sub1 (goo%-expire goo)))]))
  (define (init lst)
    (cond [(empty? lst) null]
          [(empty? (cdr lst)) null]
          [else (cons (car lst) (init (cdr lst)))]))
  (define (move-head snake)
    (define head-x (position%-x (snake%-head snake)))
    (define head-y (position%-y (snake%-head snake)))
    (define direction (snake%-direction snake))
    (cond [(equal? direction 'up) (position% head-x (sub1 head-y))]
          [(equal? direction 'down) (position% head-x (add1 head-y))]
          [(equal? direction 'left) (position% (sub1 head-x) head-y)]
          [(equal? direction 'right) (position% (add1 head-x) head-y)]))
  (define snake (pit%-snake world))
  (define goos (pit%-goos world))
  (define confront-goo (encounter-goo snake goos))
  (cond [confront-goo (pit% (snake% (snake%-direction snake) (move-head snake) (cons (snake%-head snake) (snake%-body snake)))
                            (map time-flies (cons (birth-goo) (remove confront-goo goos))))]
        [else (pit% (snake% (snake%-direction snake) (move-head snake) (cons (snake%-head snake) (init (snake%-body snake))))
                    (map time-flies goos))]))

;; render :: pit% -> IO ()
;; Handles draw event
(define (render! world)
  (define (image+scene position image scene)
    (define x (* (position%-x position) GRID-SIZE))
    (define y (* (position%-y position) GRID-SIZE))
    (pin-over scene x y image))
  (define (body+scene positions scene)
    (cond [(empty? positions) scene]
          [(empty? (cdr positions)) (image+scene (car positions) TAIL-IMG scene)]
          [else (image+scene (car positions) BODY-IMG (body+scene (cdr positions) scene))]))
  (define (goos+scene goos scene)
    (cond [(empty? goos) scene]
          [else (image+scene (goo%-location (car goos)) GOO-IMG (goos+scene (cdr goos) scene))]))
  (define (snake+scene snake scene)
    (image+scene (snake%-head snake)
                 (cadr (assoc (snake%-direction snake) HEAD-IMGS))
                 (body+scene (snake%-body snake) scene)))
  (snake+scene (pit%-snake world) (goos+scene (pit%-goos world) BG-SCENE)))

;; render-when-game-over! :: pit% -> IO ()
;; Handles draw when game over.
(define (render-when-game-over! world)
  (cc-superimpose (render! world) (text "Game over" (list (send the-color-database find-color "black")) FONT-SIZE)))

;; dead? :: pit% -> Bool
;; Tell big-bang when to stop the game
(define (dead? world)
  (define (self-colliding? snake)
    (cons? (member (snake%-head snake) (snake%-body snake))))
  (define (wall-colliding? snake)
    (define x (add1 (position%-x (snake%-head snake))))
    (define y (add1 (position%-y (snake%-head snake))))
    (or (= 0 x) (= x BOARD-LENGTH) (= 0 y) (= y BOARD-LENGTH)))
  (or (self-colliding? (pit%-snake world)) (wall-colliding? (pit%-snake world))))
