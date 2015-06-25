#lang at-exp typed/racket

#|
Discipline:         Custom big-bang for My Digital World [Typed Version]
Author:             WarGrey Gyoudmon Ju
Maintainer:         <https://github.com/wargrey/discipline>
|#

@require{../digicore.rkt}

(require typed/mred/mred)

(provide (all-from-out typed/mred/mred))

(provide (all-from-out "../digicore.rkt"))

(provide (except-out (all-defined-out)
                     before-big-bang% Digital-World<%>
                     digital-world-planet@ Digital-World-Planet@))

(require/typed racket/date
               [current-date (-> date)])

(require/typed mrlib/gif
               [write-animated-gif (->* [(Listof (Instance Bitmap%)) Natural Path-String]
                                        [#:loop? Boolean #:one-at-a-time? Boolean #:last-frame-delay (Option Natural)] Void)])

(define-type Parent% (U (Instance Frame%) (Instance Dialog%) (Instance Panel%) (Instance Pane%)))

(define-type Big-Bang-Keyword (U '#:island '#:name '#:desc
                                 '#:to-draw '#:width '#:height
                                 '#:on-tick '#:tick-rate '#:tick-limit '#:on-key '#:on-mouse
                                 '#:stop-when '#:last-draw '#:record? '#:keyframe? '#:gif-rate '#:save-label?))

(define-type Digital-World-Config@%
  {Object (field [desc String]
                 [width Nonnegative-Integer] [height Nonnegative-Integer]
                 [frame-rate Positive-Real] [frame-max (U Natural +inf.0)]
                 [record-gif? Boolean] [gif-rate (Option Positive-Real)]
                 [big-palatte? Boolean] [save-label? Boolean])})

(define digital-world-config%
  {class object%
    (super-new)
    (init-field [desc : String ""] [width : Nonnegative-Integer 1] [height : Nonnegative-Integer 1]
                [frame-rate : Positive-Real 28] [frame-max : (U Natural +inf.0) +inf.0]
                [record-gif? : Boolean #true] [gif-rate : (Option Positive-Real) #false]
                [big-palatte? : Boolean #false] [save-label? : Boolean #true])})

(define-type Digital-World<%>
  {Class [time-frame {-> (U Any 'same 'over)}]
         [interact {(U Char Symbol) Integer Integer -> (U Any 'same 'over)}]
         [keyframe? {-> Boolean}]
         [last-paint {-> (U Void (Instance Bitmap%))}]})

(define-type Digital-World%
  {Class #:implements Digital-World<%>
         (init [planet-width Nonnegative-Integer]
               [planet-height Nonnegative-Integer])
         (field [settings Digital-World-Config@%])
         [paint {-> (Instance Bitmap%)}]})

(define digital-world% : Digital-World<%>
  {class object%
    (super-new)
    
    (define/public (time-frame)
      'same)
    
    (define/public (interact event mouse-x mouse-y)
      'same)
    
    (define/public (keyframe?)
      #true)
    
    (define/public (last-paint)
      (void))})

(define before-big-bang% : Digital-World%
  {class digital-world%
    (super-new)
    (init planet-width planet-height)
    (field [settings (new digital-world-config% [desc "What happened before the Big Bang?"] [record-gif? #false])])    

    (define/public (paint)
      (define world-text (get-field desc settings))
      (define painter (make-object bitmap-dc% (make-bitmap 1 1)))
      (define font (send the-font-list find-or-create-font 16 'symbol 'normal 'bold))
      (define-values {width height distance vspace} (send painter get-text-extent world-text font))
      (send painter set-bitmap (make-bitmap (cast (exact-round width) Positive-Integer) (cast (exact-round height) Positive-Integer)))
      (send painter set-font font)
      (send painter draw-text world-text 0 0)
      (cast (send painter get-bitmap) (Instance Bitmap%)))})

(define-type Digital-World-Planet%
  {Class #:implements Vertical-Panel%
         (init [parent Parent%])
         [big-bang {Digital-World% Symbol Symbol -> Void}]})

(define digital-world-planet% : Digital-World-Planet%
  {class vertical-panel%
    (super-new [enabled #true] [style '{border}] [alignment '{left top}] [spacing 0] [border 0]
               [vert-margin 0] [horiz-margin 0] [min-width #false] [min-height #false]
               [stretchable-width #true] [stretchable-height #true])
    
    (define label : (Instance Message%) (new message% [label ""] [parent this]))
    (define shell : (Instance Vertical-Panel%) (new vertical-panel% [parent this] [style '{auto-hscroll auto-vscroll}] [alignment '{center center}]))
    (define planet : (Instance Digital-World-Planet@) (new digital-world-planet@
                                                           [parent shell] [world% before-big-bang%] [island 'void] [site 'void]
                                                           [planet-width (send shell get-width)] [planet-height (send shell get-height)]
                                                           [collapse-callback void]))
    
    (define/public (big-bang new-world% island-name site-name)
      (send shell delete-child planet)
      (set! planet (new digital-world-planet@ [parent shell] [world% new-world%] [island island-name] [site site-name]
                        [planet-width (send shell get-width)] [planet-height (send shell get-height)]
                        [collapse-callback {λ _ (void (unless (subclass? new-world% before-big-bang%) (big-bang before-big-bang% 'void 'void))
                                                      (collect-garbage))}]))
      (send label set-label (send planet get-title)))})

(define-type Digital-World-Planet@
  {Class #:implements Canvas%
         (init [parent (Instance Area-Container<%>)] [world% Digital-World%] [island Symbol] [site Symbol]
               [planet-width Nonnegative-Integer] [planet-height Nonnegative-Integer]
               [collapse-callback (-> Void)])
         [get-title (-> (U String (Instance Bitmap%)))]})

(define digital-world-planet@ : Digital-World-Planet@
  {class canvas%
    (private [render-world : ((U Any 'same 'over) -> Void)]
             [world-collapse : (-> Void)]
             [save-snapshots-if-required : (-> Void)])
    
    (init parent world% island site planet-width planet-height collapse-callback)
    
    (define planet : (Instance Digital-World%) (new world% [planet-width planet-width] [planet-height planet-height]))
    (define snapshotn : (Instance Bitmap%) (send planet paint))
    (define snapshots : (Listof (Instance Bitmap%)) (list snapshotn))
    (define on-collapse : (-> Void) collapse-callback)
    (define world-name : String (string-titlecase (string-join (string-split (format "~a" (object-name world%)) @pregexp{-|%$}))))
    (define settings : Digital-World-Config@% (get-field settings planet))
    
    (define labeldc : (Instance Bitmap-DC%) (make-object bitmap-dc% (make-bitmap 1 1)))
    (let*-values ([{lblfont} (send the-font-list find-or-create-font 14 'default 'normal 'normal)]
                  [{subfont} (send the-font-list find-or-create-font 10 'default 'italic 'normal)]
                  [{lblwidth lblheight lbldistance lblvspace} (send labeldc get-text-extent world-name lblfont)]
                  [{subwidth subheight subdistance subvspace} (send labeldc get-text-extent (get-field desc settings) subfont)])
      (send* labeldc (set-bitmap (make-bitmap (cast planet-width Positive-Integer) (cast (exact-floor (+ lblheight subheight)) Positive-Integer))))
      (send* labeldc (set-font lblfont) (draw-text world-name 0 0))
      (send* labeldc (set-font subfont) (draw-text (get-field desc settings) (max 0 (- lblwidth subwidth)) lblheight)))
    
    (define interval : Natural (cast (exact-floor (/ 1000.0 (get-field frame-rate settings))) Natural))
    (define delaycs : Natural (cast (exact-floor (/ 100.0 (or (get-field gif-rate settings) (get-field frame-rate settings)))) Natural))
    (define frame-max : (U Natural +inf.0) (get-field frame-max settings))
    (define islanddir : String (symbol->string island))
    (define sitedir : String (symbol->string site))
    
    (define clock : (Instance Timer%) (make-object timer% (cond [(infinite? frame-max) {λ _ (render-world (send planet time-frame))}]
                                                                [else {λ _ (cond [(> frame-max 0) (render-world (send planet time-frame))
                                                                                                  (set! frame-max (cast (sub1 frame-max) Natural))]
                                                                                 [else (world-collapse)])}])))
    
    ; When stretchable is false, it always uses the min-size.
    (super-new [parent parent] [enabled #true] [style '{transparent}] [paint-callback void] [label #false] [gl-config #false]
               [vert-margin 0] [min-width (max (send snapshotn get-width) (get-field width settings))] [stretchable-width #false]
               [horiz-margin 0] [min-height (max (send snapshotn get-height) (get-field height settings))] [stretchable-height #false])
    
    (define/public (get-title)
      (cast (send labeldc get-bitmap) (Instance Bitmap%)))
    
    (define/override (on-superwindow-show shown?)
      (send clock stop)
      (cond [shown? (send clock start interval)]
            [else (save-snapshots-if-required)])
      (void))
    
    (define/override (on-subwindow-char whoever key)
      (render-world (send planet interact (send key get-key-code) (send key get-x) (send key get-y)))
      #true)
    
    (define/override (on-subwindow-event whoever mouse)
      (render-world (send planet interact (send mouse get-event-type) (send mouse get-x) (send mouse get-y)))
      #true)
    
    (define/override (on-paint)
      (send (send this get-dc) draw-bitmap (car snapshots) 0 0)
      (void))
    
    {begin ; Private
      (define (render-world state)
        (cond [(equal? state 'same) (void)]
              [(equal? state 'over) (world-collapse)]
              [else (set! snapshotn (send planet paint))
                    (when (send planet keyframe?)
                      (set! snapshots (cons snapshotn snapshots)))
                    (send (send this get-dc) draw-bitmap snapshotn 0 0)
                    (void)]))
      
      (define (world-collapse)
        (define last-scene (send planet last-paint))
        (cond [(void? last-scene) (on-collapse)]
              [else (send clock stop)
                    (set! snapshotn last-scene)
                    (set! snapshots (cons snapshotn snapshots))
                    (send (send this get-dc) draw-bitmap snapshotn 0 0)
                    (void)]))
      
      (define (save-snapshots-if-required)
        (when (get-field record-gif? settings)
          (define now (let ([d (current-date)]) (format "~a-~a-~aT~a:~a:~a" (date-year d) (date-month d) (date-day d) (date-hour d) (date-minute d) (date-second d))))
          (define island-root (build-path (digimon-island) islanddir "site" sitedir))
          (define filename (build-path island-root (format "~a-~a.gif" (string-replace world-name " " "-") now)))
          (unless (directory-exists? island-root) (make-directory* island-root))
          (define bitmaps (if (memq snapshotn snapshots) snapshots (cons snapshotn snapshots)))
          (when (get-field save-label? settings)
            (for-each {λ [[bmp : (Instance Bitmap%)]] (send (make-object bitmap-dc% bmp) draw-bitmap (cast (send labeldc get-bitmap) (Instance Bitmap%)) 0 0)} bitmaps))
          (write-animated-gif (reverse bitmaps) delaycs filename #:loop? #true #:one-at-a-time? (get-field big-palatte? settings) #:last-frame-delay delaycs)))}})
