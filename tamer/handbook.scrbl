#lang scribble/manual

@require{tamer.rkt}

@(define githublink
   {lambda [target . content]
     (apply hyperlink
            (string-append "/" (current-digimon) target)
            content)})

@handbook-title[]

@deftech{Discipline} is the act of intentionally restricting your design choices
so that you can work more productively at a higher level of abstraction.

@italic{All the projects are not ready!}

@(itemlist #:style 'compact
           @item{@githublink["/digivice/land-bang.rkt"]{Land of Lisp: Learn to Program in Lisp, One Game at a Time!}}
           @item{@githublink["/digivice/realm-bang.rkt"]{Realm of Racket: Learn to Program, One Game at a Time!}}
           @item{@githublink["/digivice/nature-bang.rkt"]{The Nature of Code: Simulating Natural Systems with Racket}})

@tamer-smart-summary[]
@handbook-smart-table[]
