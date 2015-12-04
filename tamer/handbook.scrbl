#lang scribble/manual

@require{tamer.rkt}

@(define {smart-table-contents name . title}
   (nested title (itemlist (for/list ([rkt (in-list (sort (find-digimon-files (curry regexp-match? @pregexp{.+\.rkt$})
                                                                              (build-path (digimon-village) name)) path<?))])
                             (item (hyperlink (path->string (find-relative-path (digimon-zone) rkt))
                                              (string-replace (path->string (path-replace-suffix (file-name-from-path rkt) "")) #px"-+" " ")))))))

@handbook-title[]

@deftech{Discipline} is the act of intentionally restricting your design choices
so that you can work more productively at a higher level of abstraction.

@(itemlist #:style 'compact
           @smart-table-contents["land-of-lisp"]{Land of Lisp: Learn to Program in Lisp, One Game at a Time!}
           @smart-table-contents["realm-of-racket"]{Realm of Racket: Learn to Program, One Game at a Time!}
           @smart-table-contents["the-nature-of-code"]{The Nature of Code: Simulating Natural Systems with Racket})

