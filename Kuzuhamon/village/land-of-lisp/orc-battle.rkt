#lang at-exp racket

#|
Land of Lisp:       Advanced Datatypes and Generic Programming: The Orc Battle Game
Synopsis:           The Orc Battle Game
Description:        The player starts with a certain amount of health, strength, and agility. When the surviving monsters
                    have their turn, they attack the player, damage her health, weaken her strength, or reduce her agility.
                    When it is the player's turn, she gets some number of attacks and must try to kill as many monsters as
                    possible. The only good monster is a dead monster.
Author:             WarGrey Gyoudmon Ju
Maintainer:         <https://github.com/wargrey>
|#

@require{../../digitama/engine/ancient-world-x.rkt}

(require racket/generic)

(define-namespace-anchor orc-battle-space)

(define-values [HEALTH# AGILITY# STRENGTH# FOES#] (values 35 35 35 12))

(define-generics monster<%>
  [setup monster<%>]
  [desc monster<%>]
  [behit monster<%> damage]
  [attack monster<%> player]
  
  #:fallbacks
  [(define (setup this) this)
   (define (desc this) `{A fierce ,(string-upcase (symbol->string (object-name this)))})
   (define (behit this damage)
     (set-monster-health! this (- (monster-health this) damage))
     (if (>= 0 (monster-health this))
         `{You kill the ,(object-name this)!}
         `{You hit the ,(object-name this) knocking off ,damage health points!}))])

(struct player {[health #:auto] [agility #:auto] [strength #:auto]}
  #:mutable
  #:auto-value 0)

(struct monster {[health #:mutable]} #:constructor-name abstract-monster)

(struct orc monster {[club #:auto]}
  #:mutable
  #:auto-value 0
  
  #:methods gen:monster<%>
  [(define (setup this)
     (set-orc-club! this (add1 (random 8)))
      this)
   (define (desc this)
     `{A wicked orc with level ,(orc-club this) club})
   (define (attack this player)
     (let* ([attack# (add1 (random (orc-club this)))])
       (set-player-health! player (- (player-health player) attack#))
       `{An orc swings his club at you and knocks off ,attack# of your health points!}))])

(struct hydra monster {}
  #:mutable
  #:auto-value 0
  
  #:methods gen:monster<%>
  [(define (desc this)
     `{A malicious hydra with ,(monster-health this) heads})
   (define (behit this damage)
     (define heads (monster-health this))
     (set-monster-health! this (max (- heads damage) 0))
     (if (>= 0 (monster-health this))
         `{The corpse of the fully decapitated and decapitated hydra falls to the floor!}
         `{You lop off ,(min heads damage) of the "hydra's" heads!}))
   (define (attack this player)
     (let ([attack# (add1 (random (max (arithmetic-shift (monster-health this) -1) 1)))])
       (set-player-health! player (- (player-health player) attack#))
       (set-monster-health! this (add1 (monster-health this)))
       `{A hydra attacks you with ,attack# of its heads! It also grows up one more head!}))])

(struct slime-mold monster {[sliminess #:auto]}
  #:mutable
  #:auto-value 0
  
  #:methods gen:monster<%>
  [(define (setup this)
     (set-slime-mold-sliminess! this (add1 (random 5)))
      this)
   (define (desc this)
     `{A slime mold with a smiliness of ,(slime-mold-sliminess this)})
   (define (attack this player)
     (let ([attack# (add1 (random (slime-mold-sliminess this)))])
       (set-player-agility! player (- (player-agility player) attack#))
       (append `{An slime mold wraps around your legs and decreases your agility by ,attack#}
               (if (zero? (random 2))
                   (begin (set-player-health! player (sub1 (player-agility player)))
                          '{! It also squirts in your face, taking away 1 health points!})
                   '{!}))))])

(struct brigand monster {}
  #:mutable
  #:auto-value 0
  
  #:methods gen:monster<%>
  [(define (attack this player)
     (let ([best-ability (max (player-health player) (player-agility player) (player-strength player))])
       (cond [(= best-ability (player-health player)) (set-player-health! player (- (player-health player) 2))
                                                      '{A brigand hits you with his slingshot, tabking off 2 health points!}]
             [(= best-ability (player-agility player)) (set-player-agility! player (- (player-agility player) 2))
                                                       '{A brigand hits your leg with his whip, tabking off 2 agility points!}]
             [(= best-ability (player-strength player)) (set-player-strength! player (- (player-strength player) 2))
                                                        '{A brigand hits your arm with his whip, tabking off 2 strength points!}])))])

(define orc-battle%
  {class ancient-world-x% (super-new) (inherit world-read world-print)
    (define-values {knight monsters} (values (player) (make-list FOES# (void))))

    (define monster-builders (let ([orcns (namespace-anchor->namespace orc-battle-space)])
                               (filter-not void? (map {lambda [<s>] (let ([builder (namespace-variable-value <s> #false (const #false) orcns)])
                                                                      (when (and (struct-constructor-procedure? builder)
                                                                                 (= 1 (procedure-arity builder))
                                                                                 (monster<%>? (builder +NaN.0)))
                                                                        builder))}
                                                      (namespace-mapped-symbols orcns)))))
    
    (define/override (world-desc)
      "The Orc Battle Game")
    
    (define/override (big-bang!)
      (set-player-health! knight HEALTH#)
      (set-player-agility! knight AGILITY#)
      (set-player-strength! knight STRENGTH#)
      (set! monsters (map {lambda [_] (setup ((list-ref monster-builders (random (length monster-builders))) (add1 (random 10))))} monsters)))
    
    (define/override (world-over?)
      (or (player-dead?) (monsters-dead?)))
    
    (define/override (last-words)
      (cond [(player-dead?) '{You have been killed. Game Over!}]
            [(monsters-dead?) '{Congratulations! You have vanquished all of your foes!}]))
    
    (define/override (world-timeframe)
      (values 0 {lambda []
                  (newline)
                  (world-print (player-desc))
                  (for ([whocares (in-range (quotient (player-agility knight) 15))]
                        #:break (monsters-dead?))
                    (newline)
                    (world-print (monsters-desc))
                    (player-attack))
                  (unless (monsters-dead?)
                    (newline)
                    (monsters-attack))}))
    
    (define/private (orc-battle-read)
      (define action (car (world-read)))
      (when (equal? action ':q)
        (newline)
        (call-with-escape-continuation (compose1 (curryr raise #true)
                                                 (curry make-exn:break:terminate "orc-battle"
                                                        (current-continuation-marks)))))
      action)
    
    (define/private (player-dead?)
      (or (>= 0 (player-health knight))
          (>= 0 (player-agility knight))
          (>= 0 (player-strength knight))))
    
    (define/private (monster-dead? monster)
      (>= 0 (monster-health monster)))
    
    (define/private (monsters-dead?)
      (andmap {lambda [<m>] (monster-dead? <m>)} monsters))
    
    (define/private (player-desc)
      `{You are a valiant knight with a "(health, agility, strength)" of
            [,(player-health knight)"," ,(player-agility knight)"," ,(player-agility knight)]!})
    
    (define/private (monsters-desc)
      (for/fold ([foedesc '{Your foes:}]) ([i (in-range FOES#)] [monster (in-list monsters)])
        (append foedesc `{#\newline,(add1 i)"."} (if (monster-dead? monster) '{[**dead**]} `([health =,(monster-health monster)])) (desc monster))))
    
    (define/private (player-attack)
      (display "Attack Style: [s]tab, [d]ouble swing, [r]oundhouse: ")
      (case (orc-battle-read)
        [(s stab) (world-print (behit (pick-monster) (+ 3 (random (max (arithmetic-shift (player-strength knight) -1) 1)))))]
        [(d double) (let ([damage (add1 (random (quotient (player-strength knight) 6)))])
                      (world-print (behit (pick-monster) damage))
                      (unless (monsters-dead?)
                        (world-print (behit (pick-monster) damage))))]
        [(r roundhouse) (define (random-monster)
                          (define monster (list-ref monsters (random (length monsters))))
                          (if (monster-dead? monster) (random-monster) monster))
                        (for ([whocares (in-range (+ 2 (random (quotient (player-strength knight) 3))))]
                              #:break (monsters-dead?))
                          (world-print (behit (random-monster) 1)))]
        [else (player-attack)]))
    
    (define/private (monsters-attack)
      (for ([monster (in-list (filter-not {lambda [<m>] (monster-dead? <m>)} monsters))]
            #:break (player-dead?))
        (world-print (attack monster knight))))
    
    (define/private (pick-monster)
      (display "Monster#: ")
      (with-handlers ([exn:fail:contract? {lambda [efc]
                                            (world-print '{This is not a valid monster index!})
                                            (pick-monster)}])
        (define monster (list-ref monsters (sub1 (orc-battle-read))))
        (if (monster-dead? monster)
            (begin (world-print `{That ,(object-name monster) is already dead.})
                   (pick-monster))
            monster)))})
