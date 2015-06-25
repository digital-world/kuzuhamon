#lang at-exp racket

(define ~grant '{new-game! walk! charge!})

@require{../../digitama/popen/graphviz.rkt}
@require{../../digitama/engine/ancient-world.rkt}

;;; land-of-lisp: The Grand Theft Wumpus Game
;;; In this game you are the Lisp alien. You and the Wumpus have just robbed a liquor store and made off with the loot.
;;; However, during the escape, the Wumpus decides to double-cross you and run off with the money and car.
;;; But before he drives off, you manage to cap him a couple of times in the kidney.
;;; You should always be able to visit any location in Congestion City, but you need to be careful the cops which may catch you.

(define-values {congestion-city-nodes congestion-city-edges visited-nodes} (values null null null))
(define-values {node-num edge-num worm-num cop-odds player-pos} (values 32 48 4 16 #f))

(define (random-node)
  (add1 (random node-num)))

(define (edge-pair a b)
  (cond [(equal? a b) (edge-pair (random-node) (random-node))]
        [else (list (cons a b) (cons b a))]))

(define (make-edge-list)
  (apply append (for/list ([who-cares (in-range edge-num)])
                          (edge-pair who-cares who-cares))))

(define (direct-edges node edges)
  (filter {lambda [edge] (equal? (car edge) node)} edges))

(define (get-connected node edges)
  (define visited null)
  (define (traverse node)
    (unless (member node visited)
      (set! visited (cons node visited)) ; clisp: (push node visited)
      (map {lambda [edge] (traverse (cdr edge))} (direct-edges node edges))))
  (traverse node)
  visited)

(define (find-islands nodes edges)
  (define islands null)
  (define (find-island nodes)
    (define connected (get-connected (car nodes) edges))
    (define unconnected (filter-not {lambda [node] (member node connected)} nodes))
    (set! islands (cons connected islands)) ; clisp: (push connected islands)
    (unless (empty? unconnected)
      (find-island unconnected)))
  (find-island nodes)
  islands)

(define (connect-all-islands nodes edges)
  (define (connect-with-bridges islands)
    (cond [(empty? (cdr islands)) null]
          [else (append (edge-pair (caar islands) (caadr islands)) (connect-with-bridges (cdr islands)))]))
  (append (connect-with-bridges (find-islands nodes edges)) edges))

(define (edges-to-alist edges)
  (define (make-alist node)
    (cons node (map {lambda [edge] (list (cdr edge))} (remove-duplicates (direct-edges node edges) equal?))))
  (map make-alist (remove-duplicates (map car edges))))

(define (add-cops edge-alist edges-with-cops)
  (define (make-alist a-edge)
    (define-values {node edges} (values (car a-edge) (cdr a-edge)))
    (define (add-cop edge)
      (define neighbor (car edge))
      (cond [(or (member (cons node neighbor) edges-with-cops) (member (cons neighbor node) edges-with-cops)) (list neighbor 'cops)]
            [else edge]))
    (cons node (map add-cop edges)))
  (map make-alist edge-alist))

(define (make-city-edges)
  (define nodes (range 1 (add1 node-num)))
  (define edges (connect-all-islands nodes (make-edge-list)))
  (define cops (filter {lambda [who-cares] (zero? (random cop-odds))} edges))
  (add-cops (edges-to-alist edges) cops))

(define (neighbors node edge-alist)
  (map car (cdr (assoc node edge-alist))))

(define (within-one a b edge-alist)
  (member b (neighbors a edge-alist)))

(define (within-two a b edge-alist)
  (or (within-one a b edge-alist) (ormap {lambda [x] (within-one x b edge-alist)} (neighbors a edge-alist))))

(define (make-city-nodes edge-alist)
  (define-values {wumpus glow-worms} (values (random-node) (build-list worm-num {lambda [who-cares] (random-node)})))
  (for/list ([n (in-range 1 (add1 node-num))])
    (append (list n)
            (cond [(equal? n wumpus) '{wumpus}]
                  [(within-two n wumpus edge-alist) '{blood!}]
                  [else null])
            (cond [(member n glow-worms) '{glow-worms}]
                  [(ormap {lambda [worm] (within-one n worm edge-alist)} glow-worms) '{lights!}]
                  [else null])
            (cond [(ormap {lambda [edges] (empty? (cdr edges))} (cdr (assoc n edge-alist))) '{sirens!}]
                  [else null]))))

(define (find-empty-node)
  (define default-pos (random-node))
  (cond [(empty? (cdr (assoc default-pos congestion-city-edges))) (find-empty-node)]
        [else default-pos]))

(define (draw-city!)
  (graph->png "grand-theft-wumpus.png" congestion-city-nodes congestion-city-edges #:digraph #false))

(define (draw-path!)
  (define (known-city-nodes)
    (define (mark-visited node)
      (cond [(member node visited-nodes) (define n (assoc node congestion-city-nodes))
                                         (if (equal? node player-pos) (append n '{*}) n)]
            [else (list node '?)]))
    (define (search-neighbors node)
      (map car (cdr (assoc node congestion-city-edges))))
    (map mark-visited (remove-duplicates (append visited-nodes (flatten (map search-neighbors visited-nodes))))))
  (define (known-city-edges)
    (define (search node)
      (cons node (map {lambda [x] (if (member (car x) visited-nodes) x (list (car x)))} (cdr (assoc node congestion-city-edges)))))
    (map search visited-nodes))
  (graph->png "grand-theft-wumpus-path.png" (known-city-nodes) (known-city-edges) #:digraph #false))

(define (handle-new-place edge pos charging?)
  (define node (assoc pos congestion-city-edges))
  (define has-worm? (and (member 'glow-worms node) (not (member pos visited-nodes))))
  (unless (member pos visited-nodes)
    (set! visited-nodes (cons pos visited-nodes)))
  (set! player-pos pos)
  (draw-path!)
  (cond [(member 'cops edge) '{You ran into the cops. Game Over.}]
        [(member 'wumpus node) (if charging? '{You found the Wumpus!} '{You ran into the Wumpus!})]
        [charging? '{You wasted your last bullet. Game Over.}]
        [has-worm? (define new-pos (random-node))
                   (world-print `{You ran into a Glow Worm Gang! You're now at ,new-pos})
                   (handle-new-place null new-pos #false)]))

(define (handle-direction! pos charging?)
  (define edge (assoc pos (cdr (assoc player-pos congestion-city-edges))))
  (cond [edge (handle-new-place edge pos charging?)]
        [else '{That location does not exist!}]))

(define (walk! pos)
  (handle-direction! pos #false))

(define (charge! pos)
  (handle-direction! pos #true))

(define (new-game!)
  (set! congestion-city-edges (make-city-edges))
  (set! congestion-city-nodes (make-city-nodes congestion-city-edges))
  (set! player-pos (find-empty-node))
  (set! visited-nodes (list player-pos))
  (draw-city!)
  (draw-path!)
  '{Mission Start! (Refreshing the map with your favourite image viewer to explore)})
