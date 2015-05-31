#lang at-exp racket

#|
Land of Lisp:       Lisp is Hacking: Using loop to Evolve!
Synopsis:           The Darwinian Battlefield
Description:        Our world consists of a simple rectangular plane, with edges that
                    wrap around to the opposite side. Most of this world is covered in
                    steppes, meaning that very few plants grow for the animals to eat.
                    In the center of the world is a small jungle, where plants grow much
                    faster. Our animals will forage this world in search for food.
Author:             WarGrey Gyoudmon Ju
Maintainer:         <https://github.com/wargrey>
|#


@require{../../digitama/engine/ancient-world-x.rkt}

(require racket/generic)

(define darwinian-battlefield%
  {class ancient-world-x% (super-new) (inherit world-read world-print)
    (define-values {WIDTH HEIGHT JUNGLE PLANT-ENERGY REPRODUCTION-ENERGY} (values 100 30 '{40 10 32 24} 80 200 ))
    (define-values {H-EDGE V-EDGE plants herbivores days} (values (make-list (+ WIDTH 2) #\-) #\| (make-hash) null 0))

    (define/override (world-desc)
      "The Darwinian Battlefield")
    
    (define/override (big-bang!)
      (with-handlers ([exn:fail? void])
        (define-values {tput stdsize false stderr} (subprocess #false (current-input-port) #false (find-executable-path "stty") "size"))
        (when (eof-object? (read-line stderr))
          (set!-values {WIDTH HEIGHT} (values (- (read stdsize) 2) (- (read stdsize) 3)))
          (set! H-EDGE (make-list (+ WIDTH 2) (car H-EDGE))))
        (close-input-port stdsize)
        (close-input-port stderr))
      (set! days 0)
      (hash-clear! plants)
      (set! herbivores (list (new {class object% (super-new)
                                    (init-field [x (random (arithmetic-shift WIDTH -1))]
                                                [y (random (arithmetic-shift HEIGHT -1))]
                                                [energy 1000]
                                                [direction 0]
                                                [genes (build-list 8 {lambda [whocares] (add1 (random 10))})])
                                    
                                    (define/public (at-location? lx ly)
                                      (and (= x lx) (= y ly)))
                                    
                                    (define/public (dead?)
                                      (<= energy 0))
                                    
                                    (define/public (move)
                                      (set! x (remainder (+ x (case direction [{2 3 4} 1] [{1 5} 0] [else -1]) WIDTH) WIDTH))
                                      (set! y (remainder (+ y (case direction [{4 5 6} 1] [{0 1 2} -1] [else 0]) HEIGHT) HEIGHT))
                                      (set! energy (sub1 energy)))
                                    
                                    (define/public (turn)
                                      (define (angle genes x)
                                        (define xnu (- x (car genes)))
                                        (if (< xnu 0) 0 (add1 (angle (cdr genes) xnu))))
                                      (set! direction (remainder (+ (angle genes (random (apply + genes))) direction) 8)))
                                    
                                    (define/public (eat)
                                      (define location (cons x y))
                                      (when (hash-ref plants location #false)
                                        (set! energy (+ energy PLANT-ENERGY))
                                        (hash-remove! plants location)))
                                    
                                    (define/public (reproduce)
                                      (when (> energy REPRODUCTION-ENERGY)
                                        (set! energy (arithmetic-shift energy -1))
                                        (define nth (random 8))
                                        (set! herbivores (cons (make-object this% x y energy direction (append (take genes nth)
                                                                                                               (cons (max 1 (+ (list-ref genes nth) (random 3) -1))
                                                                                                                     (cdr (drop genes nth)))))
                                                               herbivores))))}))))
    
    (define/override (world-over?)
      (null? herbivores))
    
    (define/override (last-words)
      (newline)
      '{All animals are dead!})
    
    (define/override (world-timeframe)
      (define (random-plants left top width height)
        (define location (cons (+ left (random width)) (+ top (random height))))
        (hash-set! plants location #true))
      (values 1 {lambda []
                  (set! days (add1 days))
                  (set! herbivores (filter-not {lambda [animal] (send animal dead?)} herbivores))
                  (map {lambda [animal]
                         (send animal turn)
                         (send animal move)
                         (send animal eat)
                         (send animal reproduce)}
                       herbivores)
                  (apply random-plants JUNGLE)
                  (random-plants 0 0 WIDTH HEIGHT)
                  (displayln (list->string (append (cons #\newline H-EDGE) (list #\newline)
                                                   (cddr (for/fold ([texture (cons V-EDGE (cons #\newline H-EDGE))]) ([y (in-range (sub1 HEIGHT) -1 -1)])
                                                           (cons V-EDGE (cons #\newline (cons V-EDGE (for/fold ([line texture]) ([x (in-range (sub1 WIDTH) -1 -1)])
                                                                                                       (cond [(ormap {lambda [herbivory] (send herbivory at-location? x y)} herbivores) (cons #\M line)]
                                                                                                             [(hash-ref plants (cons x y) #false) (cons #\* line)]
                                                                                                             [else (cons #\space line)]))))))))))
                  (printf "Days on their own: ~a" days)}))})
