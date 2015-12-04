#lang at-exp racket/base

;;; land-of-lisp: A interface to visualize graphs by translating data structures into graphviz dot language.

(provide graph->png)

@require{../digicore.rkt}

(require racket/list
         racket/port
         racket/file
         racket/string
         racket/format
         racket/system)

(define label-maxlength 32)

(define (expression->dot-name expression)
  (define raw-bytes (string->list (string-trim (format "~s" expression))))
  (list->string (map {lambda [char] (if (or (char-alphabetic? char) (char-numeric? char)) char #\_)} raw-bytes)))

(define (expression->dot-label expression)
  (cond [(empty? expression) ""]
        [else (string-append "[label=\"" (~a (string-trim (~s expression)) #:max-width label-maxlength #:limit-marker "...") "\"]")]))

(define (nodes->dot-statements node-root)
  (map {lambda [node]
         (string-append (expression->dot-name (car node))
                        (expression->dot-label node) ";")}
       node-root))

(define (edges->dot-statements edge-root #:digraph [digraph? #true])
  (define (dot-statement node1 node2 label)
    (string-append (expression->dot-name node1)
                   (if digraph? "->" "--")
                   (expression->dot-name node2)
                   (expression->dot-label label) ";"))
  (define (digraph->dot-statement node)
    (map {lambda [edge] (dot-statement (car node) (car edge) (cdr edge))} (cdr node)))
  (define (maplist proc root)
    (cond [(empty? root) null]
          [else (cons (map proc (list root)) (maplist proc (cdr root)))]))
  (define (graph->dot-statement node)
    (map {lambda [edge] (unless (assoc (car edge) (cdr node)) (dot-statement (caar node) (car edge) (cdr edge)))} (cdar node)))
  (filter-not void? (flatten (if digraph? (map digraph->dot-statement edge-root) (maplist graph->dot-statement edge-root)))))

(define (graph->dot nodes edges #:digraph [digraph? #true])
  (define {1G=G statements [level 0]}
    (cond [(null? statements) null]
          [else (define-values {statement next} (values (string-trim (car statements)) (cdr statements)))
                (define-values {prefix postfix} (values (make-string (* level 4) #\space) (string #\newline)))
                (define pushback (string-ref statement (sub1 (string-length statement))))
                (cond [(equal? pushback #\{) (cons (string-append prefix statement postfix) (1G=G next (add1 level)))]
                      [(equal? pushback #\}) (cons (string-append (substring prefix 4) statement postfix) (1G=G next (sub1 level)))]
                      [else (cons (string-append prefix statement postfix) (1G=G next level))])]))
  (1G=G (append (list (if digraph? "digraph {" "graph {"))
                (nodes->dot-statements nodes)
                (edges->dot-statements edges #:digraph digraph?)
                (list "}"))))

(define (graph->png fpng nodes edges #:digraph [digraph? #true])
  (define dotfile (make-temporary-file))
  (define autodir (path->string (build-path (digimon-island) "land-of-lisp" "site")))
  (call-with-output-file dotfile #:exists 'replace {lambda [dot-stream]
                                                     (for/list ([statement (in-list (graph->dot nodes edges #:digraph digraph?))])
                                                       (eprintf "~a" statement)
                                                       (fprintf dot-stream "~a~n" statement))})
  (system (format "dot -Tpng -o~s/~s ~s" autodir fpng (path->string dotfile))))
