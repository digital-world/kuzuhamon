#lang at-exp racket/gui

#|
Discipline:         Custom big-bang for My Digital World [Untyped Version]
Author:             WarGrey Gyoudmon Ju
Maintainer:         <https://github.com/wargrey/discipline>
|#

(provide (all-defined-out))
(provide (all-from-out "../digicore.rkt"))

@require{../digicore.rkt}

(require racket/date)
(require images/flomap)
(require mrlib/gif)
(require pict)

;;; Custom Contracts

(define world-state/c
  any/c)

(define key-event/c
  (or/c char? key-code-symbol?))

(define mouse-event/c
  (or/c 'enter 'leave 'left-down 'left-up
        'middle-down 'middle-up
        'right-down 'right-up 'motion))

(define world-constructor/c
  (or/c [-> world-state/c]
        [(is-a?/c dc<%>) (and/c positive? fixnum?) (and/c positive? fixnum?) . -> . world-state/c]))

(define world-painter/c
  (or/c [world-state/c . -> . (or/c pict? flomap? (is-a?/c bitmap%))]
        [world-state/c (is-a?/c bitmap-dc%) (and/c positive? fixnum?) (and/c positive? fixnum?) . -> . void?]))

;;; End Contracts

(struct stopped-world {state} #:transparent #:constructor-name stop-with)

(define/contract big-bang {[world-constructor/c #:island symbol?]
                           [#:name any/c #:desc any/c
                                   #:to-draw world-painter/c #:width (and/c positive? fixnum?) #:height (and/c positive? fixnum?)
                                   #:on-tick (world-state/c . -> . world-state/c) #:tick-rate (and/c positive? rational?) #:tick-limit exact-nonnegative-integer?
                                   #:on-key (world-state/c key-event/c . -> . world-state/c) #:on-mouse (world-state/c fixnum? fixnum? mouse-event/c . -> . world-state/c)
                                   #:stop-when (world-state/c . -> . boolean?) #:last-draw world-painter/c
                                   #:record? any/c #:keyframe? (world-state/c . -> . boolean?) #:gif-rate (and/c positive? fixnum?) #:save-label? boolean?]
                           . ->* . void?}
  {lambda {construtor
           #:island island #:name [title "Digital World"] #:desc [subtitle ""]
           #:to-draw [paint void] #:width [stage-width 1] #:height [stage-height 1]
           #:on-tick [enter-frame #false] #:tick-rate [frame-rate 28] #:tick-limit [frame-max +inf.0]
           #:on-key [key-handler #false] #:on-mouse [mouse-handler #false]
           #:stop-when [end? (const #false)] #:last-draw [last-paint #false]
           #:record? [saveas #false] #:keyframe? [keyframe? #false] #:gif-rate [delay 0] #:save-label? [label? #false]}
    (define interval (exact-floor (/ 1000 frame-rate)))
    (define painter (send (make-bitmap stage-width stage-height) make-dc))
    (define world0 (if (zero? (procedure-arity construtor)) (construtor) (construtor painter stage-width stage-height)))
    (define-values {paint-scene snapshot-scene} (cond [(equal? (procedure-arity paint) 1) (values paint identity)]
                                                      [else (values {lambda [world-state]
                                                                      (define-values {pen brush} (values (send painter get-pen) (send painter get-brush)))
                                                                      (paint world-state painter stage-width stage-height)
                                                                      (send* painter (set-pen pen) (set-brush brush))
                                                                      (send painter get-bitmap)}
                                                                    (compose1 pict->bitmap bitmap))]))
    (define open-scene (paint-scene world0))
    (cond [(pict? open-scene) (set! open-scene (pict->bitmap open-scene))
                              (set! paint-scene (compose1 pict->bitmap paint))]
          [(flomap? open-scene) (set! open-scene (flomap->bitmap open-scene))
                                (set! paint-scene (compose1 flomap->bitmap paint))])
    (send painter set-bitmap open-scene)
    (send* (make-object
               {class* frame% []
                 (super-new [label (~a title)] [parent #false]
                            [stretchable-width #false] [stretchable-height #false]
                            [min-width (max stage-width (exact-floor (send open-scene get-width)))]
                            [min-height (max stage-height (exact-floor (send open-scene get-height)))])
                 
                 (field [world-state world0]
                        [snapshots (list (snapshot-scene open-scene))]
                        [timer (make-object timer% {thunk (on-tick)})]
                        [stage (new {class canvas%
                                      (super-new)
                                      (define/override (on-paint)
                                        ;; why it starts with calling this method twice.
                                        (send (send this get-dc) draw-bitmap (last snapshots) 0 0))}
                                    [parent this])])
                 
                 (define/private (on-big-bang . args)
                   (define worldn (apply (car args) world-state (cdr args)))
                   (set! world-state (if (stopped-world? worldn) (stopped-world-state worldn) worldn))
                   (define world-scene (paint-scene world-state))
                   (when (and saveas (procedure? keyframe?) (keyframe? world-state))
                     (set! snapshots (cons (snapshot-scene world-scene) snapshots)))
                   (send (send stage get-dc) draw-bitmap world-scene 0 0)
                   (when (or (stopped-world? worldn) (end? world-state))
                     (cond [(procedure? last-paint) (send timer stop)
                                                    (define last-scene (cond [(equal? (procedure-arity last-paint) 1) (last-paint world-state)]
                                                                             [else (last-paint world-state painter stage-width stage-height)
                                                                                   (send painter get-bitmap)]))
                                                    (define last-bitmap (cond [(pict? last-scene) (pict->bitmap last-scene)]
                                                                              [(flomap? last-scene) (flomap->bitmap last-scene)]
                                                                              [else last-scene]))
                                                    (set! snapshots (cons last-bitmap snapshots))
                                                    (send (send stage get-dc) draw-bitmap last-bitmap 0 0)]
                           [else (send this show #false)])))
                 
                 (define/private (record-into dir-name)
                   (define island-root (simplify-path (build-path (digimon-island) (symbol->string island) "site" (~a dir-name))))
                   (define file-name (build-path island-root (format "~a-~a.~a" (string-replace (~a title) " " "-") (date->string (current-date) #true) (if (false? keyframe?) 'png 'gif))))
                   (define ptitle (text (~a title) null 18))
                   (define psubtitle (text (~a subtitle) '{italic} 12))
                   (define bitmaps (if (or (procedure? last-paint) (and (procedure? keyframe?) (keyframe? world-state))) snapshots (cons (snapshot-scene (paint-scene world-state)) snapshots)))
                   (unless (directory-exists? island-root) (make-directory* island-root))
                   (when label?
                     (for-each {lambda [painter]
                                 (draw-pict ptitle painter 0 0)
                                 (draw-pict psubtitle painter (max 0 (- (pict-width ptitle) (pict-width psubtitle))) (pict-height ptitle))}
                               (map {lambda [bmp] (send bmp make-dc)} bitmaps)))
                   (cond [(false? keyframe?) (send (car bitmaps) save-file file-name 'png)]
                         [else (define delay-csec (exact-round (/ 100 (if (zero? delay) frame-rate delay))))
                               (write-animated-gif (reverse bitmaps) delay-csec file-name #:loop? #true #:one-at-a-time? #true #:last-frame-delay (* delay-csec 4))]))
                 
                 (define/private (on-tick)
                   (when (frame-max . > . 0)
                     (on-big-bang enter-frame))
                   (unless (infinite? frame-max)
                     (set! frame-max (sub1 frame-max))
                     (when (frame-max . <= . 0)
                       (on-big-bang stop-with))))
                 
                 (define/override (on-subwindow-char whoever key)
                   (unless (false? key-handler)
                     (on-big-bang key-handler (send key get-key-code))))
                 
                 (define/override (on-subwindow-event whoever mouse)
                   (unless (false? mouse-handler)
                     (on-big-bang mouse-handler (send mouse get-x) (send mouse get-y) (send mouse get-event-type))))
                 
                 (define/override (on-message message)
                   (case message
                     [{get-current-mouse-state} (define-values {mouse _} (get-current-mouse-state))
                                                (define-values {mx my} (send stage screen->client (send mouse get-x) (send mouse get-y)))
                                                (list mx my)]
                     [else #| heart-beat-package |# (object-name world-state)]))
                 
                 (define/override (on-superwindow-show shown?)
                   (send timer stop)
                   (cond [shown? (unless (false? enter-frame) (send timer start interval))]
                         [saveas (record-into saveas)]))})
      (show #true) (center))})
