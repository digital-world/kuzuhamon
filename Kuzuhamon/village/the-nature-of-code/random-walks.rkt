#lang at-exp typed/racket

(define ~nature-bang @pregexp{^Bonus:[^%]+%$|Walker%$})

#|
The Nature of Code: Introduction: The Random Walker
Synopsis:           The random walk is one of the best-known and simplest simulations of motion.
Description:        Imagine you are standing in the middle of a balance beam. Every 10s you flip a coin, then stepping off that balance beam and onto the floor.
Author:             WarGrey Gyoudmon Ju
Maintainer:         <https://github.com/wargrey/discipline>
|#

@require{../../digitama/engine/digital-world.rkt}
@require{../../digitama/engine/random.rkt}

(require racket/fixnum)

(require plot)
(require images/flomap)
(require typed/pict)

(require math/flonum
         math/distributions)

;;; Begin Legacy Stub
(struct PRWalker ([x : Real] [y : Real] [pdfs-monte-carlo : (Listof Flonum)] [dh-pseudo : (HashTable Fixnum Fixnum)]))
(define Pseudo-Random-Walker% : (case-> [-> (Pairof Procedure HashTableTop)])
  {lambda []
    (define settings : (case-> [-> (HashTable Big-Bang-Keyword Any)])
      {lambda [] (make-hash `{{#:desc . "Exponentially Likelihood stepsize"}
                              {#:to-draw . ,draw}
                              {#:on-tick . ,step}
                              {#:keyframe? . ,keyframe?}
                              {#:gif-rate . 2}})})
    
    (define group : Positive-Fixnum 64)
    (define qualifying : (case-> [Flonum -> Flonum]) {lambda [monte-carlo] (flexpt monte-carlo 2.0)})
    
    (define dh-get : (case-> [PRWalker Fixnum -> Fixnum])
      {lambda [beam grp]
        (hash-ref (PRWalker-dh-pseudo beam) grp (const 0))})
    
    (define dh-inc! : (case-> [PRWalker Fixnum -> PRWalker])
      {lambda [beam grp]
        (hash-set! (PRWalker-dh-pseudo beam) grp (fx+ 1 (dh-get beam grp)))
        beam})
    
    (define construct : (case-> [(Instance Bitmap-DC%) Positive-Fixnum Positive-Fixnum -> PRWalker])
      {lambda (painter width height)
        (define x : Real (/ width 2))
        (define y : Real (/ height 4))
        (send* painter (set-pen "White" 0 'solid) (draw-rectangle 0 0 width height))
        (send painter set-pen "Black" 0 'solid)
        (PRWalker x y null (make-hash))})
    
    (define draw : (case-> [PRWalker (Instance Bitmap-DC%) Positive-Fixnum Positive-Fixnum -> Void])
      {lambda [beam painter width height]
        (define half-width : Positive-Fixnum (cast (fxquotient width 2) Positive-Fixnum))
        (define half-height : Positive-Fixnum (cast (fxquotient height 2) Positive-Fixnum))
        (unless (null? (PRWalker-pdfs-monte-carlo beam))
          (send painter draw-bitmap (plot-bitmap (list (discrete-histogram (in-list (for/list : (Listof (List Integer Real)) ([i : Integer (in-range group)])
                                                                                      (list (remainder i 10) (dh-get beam (cast i Fixnum))))) #:color 2 #:line-color 2))
                                                 #:width half-width #:height half-height #:x-label #false #:y-label #false #:x-min 0 #:y-max 128
                                                 #:title (format "(random-pseudo ~a) within ~a flippings" group (length (PRWalker-pdfs-monte-carlo beam)))) 0 half-height)
          (send painter draw-bitmap (plot-bitmap (list (density (PRWalker-pdfs-monte-carlo beam) #:color 4))
                                                 #:width half-width #:height half-height #:y-label #false #:x-label #false #:x-max 1.0 #:x-min 0.0 #:y-max 3.0
                                                 #:title "Density of (random-montecarlo (λ [p] (flexpt p 2.0))") half-width half-height))
        (when ((PRWalker-y beam) . < . half-height)
          (send painter draw-point (PRWalker-x beam) (PRWalker-y beam)))})
    
    (define keyframe? : (case-> [PRWalker -> Boolean])
      {lambda [beam]
        (zero? (remainder (length (PRWalker-pdfs-monte-carlo beam)) 500))})
    
    (define step : (case-> [PRWalker -> PRWalker])
      {lambda [beam]
        (let-values: ([{[rx : Fixnum] [ry : Fixnum]} (values (random-pseudo group) (random-pseudo group))]
                      [{[dx : Flonum] [dy : Flonum]} (values (random-montecarlo qualifying) (random-montecarlo qualifying))])
          (PRWalker ((if (even? rx) + -) (PRWalker-x beam) (* dx 10))
                    ((if (even? ry) - +) (PRWalker-y beam) (* dy 10))
                    (cons dx (cons dy (PRWalker-pdfs-monte-carlo beam)))
                    (PRWalker-dh-pseudo (dh-inc! (dh-inc! beam rx) ry))))})
    
    (cons construct (settings))})

(struct MWalker ([x : Real] [y : Real] [rallies : (Listof (Listof Real))] [keyframe? : Boolean]))
(define 64%-Mouseward-Walker% : (case-> [-> (Pairof Procedure HashTableTop)])
  {lambda []
    (define settings : (case-> [-> (HashTable Big-Bang-Keyword Any)])
      {lambda [] (make-hash `{{#:name . "Mouse Attracted Walker"}
                              {#:desc . "Probability: 64%"}
                              {#:to-draw . ,draw}
                              {#:on-tick . ,step}
                              {#:on-mouse . ,set-rally-point}
                              {#:keyframe? . ,MWalker-keyframe?}
                              {#:gif-rate . 2}})})
    
    (define worldxy : (List Integer Integer) (list 0 0))
    (define probability : Flonum 0.64)
    (define width : Positive-Fixnum 640)
    (define height : Positive-Fixnum 600)
    (define walker : (Instance Bitmap-DC%) (make-object bitmap-dc% (make-bitmap width height #false)))
    
    (define construct : (case-> [-> MWalker])
      {lambda []
        (define x : Real (/ width 2))
        (define y : Real (/ height 2))
        (MWalker x y null #true)})
    
    (define draw : (case-> [MWalker -> pict])
      {lambda [beam]
        (send walker draw-point (MWalker-x beam) (MWalker-y beam))
        (pin-over (bitmap (cast (send walker get-bitmap) (Instance Bitmap%))) 0 0 
                  (dc {lambda: ([painter : (Instance DC<%>)] [ox : Real] [oy : Real])
                        (for ([rally : (Listof Real) (in-list (MWalker-rallies beam))])
                          (send painter set-pen (if (eq? rally (car (MWalker-rallies beam))) "SpringGreen" "OrangeRed") 2 'solid)
                          (send painter draw-ellipse (- (car rally) 3 ox) (- (cadr rally) 3 oy) 6 6))}
                      width height))})
    
    (define set-rally-point : (case-> [MWalker Fixnum Fixnum Symbol -> MWalker])
      {lambda [beam x y event]
        (case event
          [{left-up} (struct-copy MWalker beam [rallies (append (MWalker-rallies beam) (list (list x y)))] [keyframe? #false])]
          [{right-up} (struct-copy MWalker beam [rallies null] [keyframe? #true])]
          [else beam])})
    
    (define step : (case-> [MWalker -> MWalker])
      {lambda [beam]
        (define r0 : (Listof (Listof Real)) (MWalker-rallies beam))
        (define-values {#{x : Real} #{y : Real}} (values (MWalker-x beam) (MWalker-y beam)))
        (define-values {#{rx : Flonum} #{ry : Flonum}} (values (random-pseudo) (random-pseudo)))
        (define-values {#{rx%2 : Integer} #{ry%2 : Integer}} (values (remainder (exact-round (* rx 100)) 2) (remainder (exact-round (* ry 100)) 2)))
        (define-values {#{mx : Real} #{my : Real}} (cond [(null? r0) (when (void (send-message-to-window (car worldxy) (cadr worldxy) 'heart-beat-package))
                                                                       (for ([parent : (U (Instance Frame%) (Instance Dialog%)) (in-list (get-top-level-windows))]
                                                                             #:when (equal? 'MWalker (send-message-to-window (send parent get-x) (send parent get-y) (void))))
                                                                         (set! worldxy (list (send parent get-x) (send parent get-y)))))
                                                                     (define mouse : (Listof Real) (cast (send-message-to-window (car worldxy) (cadr worldxy) 'get-current-mouse-state) (Listof Real)))
                                                                     (values (car mouse) (cadr mouse))]
                                                         [else (values (caar (MWalker-rallies beam)) (cadar (MWalker-rallies beam)))]))
        (define both-must-stay : Flonum (flsqrt probability))
        (define one-must-close : Flonum probability)
        (define each-should-close : Flonum (fl- 1.0 (flsqrt (fl- 1.0 probability))))
        
        (define-values {#{rn : (Listof (Listof Real))} #{k? : Boolean}} (cond [(and (= x mx) (= y my)) (values (if (null? r0) null (cdr r0)) #true)]
                                                                              [else (values (MWalker-rallies beam) #false)]))
        (define-values {#{xn : Real} #{yn : Real}} (cond [(and (= x mx) (< y my)) (values (cond [(< rx each-should-close) x] [(= rx%2 0) (sub1 x)] [else (add1 x)])
                                                                                          (if (< ry one-must-close) (add1 y) (- y ry%2)))]
                                                         [(and (= x mx) (> y my)) (values (cond [(< rx each-should-close) x] [(= rx%2 0) (sub1 x)] [else (add1 x)])
                                                                                          (if (< ry one-must-close) (sub1 y) (+ y ry%2)))]
                                                         [(and (< x mx) (= y my)) (values (if (< rx one-must-close) (add1 x) (- x rx%2))
                                                                                          (cond [(< ry each-should-close) y] [(= ry%2 0) (sub1 y)] [else (add1 y)]))]
                                                         [(and (> x mx) (= y my)) (values (if (< rx one-must-close) (sub1 x) (+ x rx%2))
                                                                                          (cond [(< ry each-should-close) y] [(= ry%2 0) (sub1 y)] [else (add1 y)]))]
                                                         [(and (< x mx) (< y my)) (values (if (< rx each-should-close) (add1 x) (- x (ceiling rx%2)))
                                                                                          (if (< ry each-should-close) (add1 y) (- y (ceiling ry%2))))]
                                                         [(and (> x mx) (> y my)) (values (if (< rx each-should-close) (sub1 x) (+ x (ceiling rx%2)))
                                                                                          (if (< ry each-should-close) (sub1 y) (+ y (ceiling ry%2))))]
                                                         [(and (< x mx) (> y my)) (values (if (< rx each-should-close) (add1 x) (- x (ceiling rx%2)))
                                                                                          (if (< ry each-should-close) (sub1 y) (+ y (ceiling ry%2))))]
                                                         [(and (> x mx) (< y my)) (values (if (< rx each-should-close) (sub1 x) (+ x (ceiling rx%2)))
                                                                                          (if (< ry each-should-close) (add1 y) (- y (ceiling ry%2))))]
                                                         [else (values (cond [(< rx both-must-stay) x] [(= rx%2 0) (sub1 x)] [else (add1 x)])
                                                                       (cond [(< ry both-must-stay) y] [(= ry%2 0) (sub1 y)] [else (add1 y)]))]))
        
        (MWalker xn yn rn k?)})
    
    (cons construct (settings))})

(struct: BSWalker ([x : Real] [y : Real] [color : (Instance Color%)] [splatter : (Listof Flonum)]
                              [palette-r : (Listof Flonum)] [palette-g : (Listof Flonum)] [palette-b : (Listof Flonum)] [errors : (Listof (Vector Flonum Flonum))]))
(define Beauty-Splatters-Walker% : (case-> [-> (Pairof Procedure HashTableTop)])
  {lambda []
    (define settings : (case-> [-> (HashTable Big-Bang-Keyword Any)])
      {lambda [] (make-hash `{{#:to-draw . ,draw}
                              {#:on-tick . ,step}
                              {#:keyframe? . ,keyframe?}
                              {#:gif-rate . 2}})})
    
    (define-values {#{width : Positive-Fixnum} #{height : Positive-Fixnum}} (values 640 300))
    (define-values {#{x0 : Positive-Fixnum} #{y0 : Positive-Fixnum}} (values (cast (fxquotient width 2) Positive-Fixnum) (cast (fxquotient height 2) Positive-Fixnum)))
    (define-values {#{μ : Flonum} #{σ : Nonnegative-Flonum}} (values 0.0 (cast (/ (fxmin x0 y0) 3.0) Nonnegative-Flonum)))
    (define-values {#{mean : Flonum} #{stddev : Flonum}} (values 128.0 42.7))
    (define frame-gradient : Byte (cast (random-pseudo 256) Byte))
    (define frame-color : (Instance Color%) (make-color frame-gradient frame-gradient frame-gradient 0.16))
    (define painter : (Instance Bitmap-DC%) (make-object bitmap-dc% (pict->bitmap (linestyle 'dot (colorize (cc-superimpose (colorize (filled-rectangle width height) "White")
                                                                                                                            (disk (* σ 1)) (disk (* σ 3)) (disk (* σ 6))) frame-color)))))
    
    (define construct : (case-> [-> BSWalker])
      {lambda []
        (BSWalker +nan.0 -nan.0 frame-color null null null null null)})
    
    (define draw : (case-> [BSWalker -> pict])
      {lambda [beam]
        (cond [(nan? (BSWalker-x beam)) (send painter draw-point x0 y0)]
              [else (send painter set-pen (BSWalker-color beam) 2 'solid)
                    (send painter draw-point (BSWalker-x beam) (BSWalker-y beam))])
        (vc-append (bitmap (cast (send painter get-bitmap) (Instance Bitmap%)))
                   (cast (plot-pict (list (x-axis 0.01 #:alpha 0.5)
                                          (if ((length (BSWalker-splatter beam)) . > . 2) (density (BSWalker-splatter beam) #:label (format "Splatter(~a, ~a)" μ σ) #:color 'Orange) null)
                                          (if ((length (BSWalker-palette-r beam)) . > . 2) (density (BSWalker-palette-r beam) #:label (format "Red(~a, ~a)" mean stddev) #:color 'Red) null)
                                          (if ((length (BSWalker-palette-g beam)) . > . 2) (density (BSWalker-palette-g beam) #:label (format "Green(~a, ~a)" mean stddev) #:color 'Green) null)
                                          (if ((length (BSWalker-palette-b beam)) . > . 2) (density (BSWalker-palette-b beam) #:label (format "Blue(~a, ~a)" mean stddev) #:color 'Blue) null)
                                          (points (BSWalker-errors beam) #:label "Palette Overflow" #:color 'DarkRed))
                                    #:width width #:height height #:x-label #false #:y-label #false #:x-min -256 #:x-max 300 #:y-min 0 #:y-max 0.012
                                    #:title (format "Normal Distribution (Samples: ~a)" (length (BSWalker-splatter beam)))) pict))})
    
    (define step : (case-> [BSWalker -> BSWalker])
      {lambda [beam]
        (define-values {#{dx : Flonum} #{dy : Flonum}} (values (random-gaussian μ σ) (random-gaussian μ σ)))
        (define-values {#{ir : Flonum} #{ig : Flonum} #{ib : Flonum}} (values (random-gaussian mean stddev) (random-gaussian mean stddev) (random-gaussian mean stddev)))
        (define-values {#{err : (Listof Flonum)} #{rgb0 : (Listof Flonum)}} (partition {lambda: ([i : Flonum]) (or (i . fl> . 255.0) (i . fl< . 0.0))} (list ir ig ib)))
        (define rgb : (Listof Byte) (map {lambda: ([i : Flonum]) (cast (exact-round i) Byte)} rgb0))
        (BSWalker (+ x0 dx) (- y0 dy)
                  (if (null? err) (make-color (list-ref rgb 0) (list-ref rgb 1) (list-ref rgb 2)) (BSWalker-color beam))
                  (cons dx (cons dy (BSWalker-splatter beam)))
                  (append (BSWalker-palette-r beam) (if (member ir err) null (list ir)))
                  (append (BSWalker-palette-g beam) (if (member ig err) null (list ig)))
                  (append (BSWalker-palette-b beam) (if (member ib err) null (list ib)))
                  (append (BSWalker-errors beam) (map {lambda: ([e : Flonum]) (vector e (flnormal-pdf mean stddev e #false))} err)))})
    
    (define keyframe? : (case-> [BSWalker -> Boolean])
      {lambda [beam]
        (zero? (remainder (length (BSWalker-splatter beam)) 500))})
    
    (cons construct (settings))})

;;; End Legacy Stub



(define Bonus:Perlin-Noise-Plot% : (case-> [-> (Pairof Procedure HashTableTop)])
  {lambda []
    (define settings : (case-> [-> (HashTable Big-Bang-Keyword Any)])
      {lambda [] (make-hash `{{#:save-label? . #false}
                              {#:to-draw . ,draw-gauge}
                              {#:last-draw . ,draw-100%}
                              {#:on-tick . ,step}
                              {#:tick-rate . ,128}
                              {#:tick-limit . 256}})})
    
    (define-values {#{width : Positive-Fixnum} #{height : Positive-Fixnum}} (values 800 300))
    (define delta : Real 0.01)
    (define pipe-classic : (Channelof (Listof (Sequenceof Real))) (make-channel))
    (define pipe-simplex : (Channelof (Listof (Sequenceof Real))) (make-channel))
    
    (define construct : (case-> [-> (Listof (Listof (Sequenceof Real)))])
      {lambda []
        (thread (thunk (for ([row : Flonum (in-range 0.0 256.0 1.0)]
                             [tr : Flonum (in-range 0.0 +inf.f delta)])
                         (channel-put pipe-classic (for/list : (Listof (Sequenceof Real)) ([col : Flonum (in-range 0.0 256.0 1.0)]
                                                                                           [tc : Flonum (in-range 0.0 +inf.f delta)])
                                                     (in-list (list row col (noise-classic tr tc))))))))
        (thread (thunk (for ([row : Flonum (in-range 255.0 -1.0 -1.0)]
                             [tr : Flonum (in-range 0.0 +inf.f delta)])
                         (channel-put pipe-simplex (for/list : (Listof (Sequenceof Real)) ([col : Flonum (in-range 255.0 -1.0 -1.0)]
                                                                                           [tc : Flonum (in-range 0.0 +inf.f delta)])
                                                     (in-list (list row col (random-simplex tr tc))))))))
        (list null null null)})
    
    (define draw-gauge : (case-> [(Listof (Listof (Sequenceof Real))) -> pict])
      {lambda [datum]
        (cc-superimpose (blank width (+ height height))
                        (cast (plot-pict (list (points (in-list (first datum)) #:color 'SeaGreen #:size 1 #:line-width 0))
                                         #:width width #:height height #:y-label #false #:title #false 
                                         #:x-min 0.0 #:x-max 256.0 #:y-min -0.5 #:y-max 0.5
                                         #:x-label (format "Generating (~a%)" (if (null? (first datum)) 0 (exact-round (* (sequence-ref (last (first datum)) 0) 0.39))))) pict))})
    
    (define step : (case-> [(Listof (Listof (Sequenceof Real))) -> (Listof (Listof (Sequenceof Real)))])
      {lambda [datum]
        (define noise2d : (Listof (Sequenceof Real)) (channel-get pipe-classic))
        (define simplex2d : (Listof (Sequenceof Real)) (channel-get pipe-simplex))
        (define start : Real (sequence-ref (car noise2d) 0))
        (define noise1d : (Listof (Sequenceof Real)) (for/list : (Listof (Sequenceof Real)) ([interx : Real (in-range start (add1 start) delta)])
                                                       (in-list (list (cast interx Flonum) (noise-classic (cast interx Flonum))))))
        (list (append (first datum) noise1d) (append (second datum) noise2d) (append (third datum) simplex2d))})
    
    (define draw-100% : (case-> [(Listof (Listof (Sequenceof Real))) -> pict])
      {lambda [datum]
        (vc-append (cast (plot-pict (list (lines (in-list (first datum)) #:color 'SeaGreen #:width 0)
                                          (function (const +0.5) #:color 'DarkSeaGreen #:style 'dot)
                                          (function (const -0.5) #:color 'DarkSeaGreen #:style 'dot))
                                    #:width width #:height height #:x-label #false #:y-label #false
                                    #:x-min 0.0 #:x-max 256.0 #:y-min -1.0 #:y-max 1.0
                                    #:title (format "Perlin Noise 1D (Delta: ~a)" delta)) pict)
                   (cast (plot3d-pict (list (points3d (in-list (second datum)) #:color 'ForestGreen #:size 1 #:line-width 0 #:label "Classic")
                                            (points3d (in-list (third datum)) #:color 'OrangeRed #:size 1 #:line-width 0 #:label "Simplex")
                                            (surface3d (const +0.5) #:color 'DarkSeaGreen #:style 'transparent #:alpha 0.5)
                                            (surface3d (const -0.5) #:color 'DarkSeaGreen #:style 'transparent #:alpha 0.5))
                                      #:width width #:height height #:x-label #false #:y-label #false #:z-label #false
                                      #:x-min 0.0 #:x-max 256.0 #:y-min 0.0 #:y-max 255.0 #:z-min -1.0 #:z-max 1.0 #:altitude 8
                                      #:title (format "Perlin Noise 2D (Delta: ~a)" delta)) pict))})
    
    (cons construct (settings))})

(define Bonus:Perlin-Noise-Cloud% : (case-> [-> (Pairof Procedure HashTableTop)])
  {lambda []
    (define settings : (case-> [-> (HashTable Big-Bang-Keyword Any)])
      {lambda [] (make-hash `{{#:save-label? . #false}
                              {#:to-draw . ,draw}
                              {#:on-tick . ,identity}
                              {#:keyframe? . ,(const #true)}
                              {#:tick-rate . 1000}})})
    
    (define width : Fixnum 640)
    (define height : Fixnum 400)
    (define component : Fixnum 3)
    (define delta : Flonum 0.02)
    (define tasks : Fixnum 4)
    
    (define render-region : {case-> [FlVector Flonum Fixnum Fixnum Fixnum Flonum -> Void]}
      {lambda [pixels now width start end fstart]
        (for ([x : Integer (in-range width)]
              [tx : Flonum (in-range 0.0 +inf.0 delta)])
          (for ([y : Integer (in-range start end)]
                [ty : Flonum (in-range fstart +inf.0 delta)])
            (define xy : Fixnum (fx* (fx+ (fx* y width) x) component))
            (define gradient : Flonum (fl+ 0.5 (noise-classic tx ty now)))
            (flvector-set! pixels (fx+ 0 xy) gradient)
            (flvector-set! pixels (fx+ 1 xy) gradient)))})
    
    (define construct : (case-> [-> FlVector])
      {lambda []
        (make-flvector (* width height component) 1.0)})
    
    (define draw : (case-> [FlVector -> flomap])
      {lambda [pixels]
        (define now : Flonum (fl* (->fl (current-process-milliseconds)) 1.0e-4))
        (define factor : Fixnum (fxquotient height tasks))
        (for-each (inst touch Void)
                  (for/list : (Listof (Futureof Void)) ([i : Integer (in-range tasks)])
                    (define start : Fixnum (fx* factor i))
                    (define end : Fixnum (fx* factor (add1 i)))
                    (future {lambda [] (render-region pixels now width start end (fl* delta (->fl start)))})))
        (flomap pixels component width height)})
    
    (cons construct (settings))})
