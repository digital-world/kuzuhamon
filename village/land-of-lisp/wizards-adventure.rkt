#lang at-exp racket

(define ~grant '{look inventory walk! pickup! dropdown!})

@require{../../digitama/popen/graphviz.rkt}

;;; land-of-lisp: The Wizard's Adventure Game
;;; In this game, you are a wizard's apprentice. You'll explore the wizard's house.
;;;     We can visit three different locations: a living room, an attic, and a garden.
;;;     We can move between places using the door and ladder to the attic.
;;;     We can pickup and dropdown the objects if there are.

(define location 'living-room)

(define game-nodes '[[living-room [You are in the living room. A wizard is snoring loudly in the couch.]]
                     [garden [You are in the beautiful garden. There is a well in front of you.]]
                     [attic [You are in the attic. There is a giant welding torch in the corner.]]])

(define game-edges '[[living-room [garden west door] [attic upstairs ladder]]
                     [garden [living-room east door]]
                     [attic [living-room downstairs ladder]]])

(define game-objects '[[whiskey [living-room]]
                       [bucket [living-room]]
                       [frog [garden]]
                       [chain [garden]]])

(define (describe-location location nodes)
  (cadr (assoc location nodes)))

(define (describe-paths location edges)
  (map {lambda [edge] `[There is a ,(caddr edge) going ,(cadr edge) from here.]} (cdr (assoc location edges))))

(define (objects-at location objects)
  (filter {lambda [object] (member location (cadr (assoc object objects)))} (map car objects)))

(define (describe-objects location objects)
  (map {lambda [object] `[You see a ,object on the floor.]} (objects-at location objects)))

(define (look)
  (flatten `[,(describe-location location game-nodes)
             ,(describe-paths location game-edges)
             ,(describe-objects location game-objects)]))

(define (walk! direction)
  (define next (filter {lambda [path] (equal? direction (cadr path))} (cdr (assoc location game-edges))))
  (cond [(empty? next) '[You cannot go that way!]]
        [else (set! location (caar next))
              (look)]))

(define (pickup! object)
  (cond [(member object (objects-at location game-objects)) (set! game-objects (append `((,object (CARRYING))) (remove `(,object (,location)) game-objects)))
                                                            `[Now you are carrying the ,object]]
        [else `[You cannot get that!]]))

(define (dropdown! object)
  (cond [(member object (objects-at 'CARRYING game-objects)) (set! game-objects (append `((,object (,location))) (remove `(,object (CARRYING)) game-objects)))
                                                             `[You dropped down the ,object]]
        [else `[You have not get that!]]))

(define (inventory)
  (define picked (objects-at 'CARRYING game-objects))
  (cond [(null? picked) '[You are not carrying anything!]]
        [else (cons 'Item- picked)]))
