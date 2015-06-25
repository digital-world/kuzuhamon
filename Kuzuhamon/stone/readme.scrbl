#lang scribble/manual

@(require "../digitama/digicore.rkt")
@(require (submod "../../DigiGnome/digitama/tamer.rkt" digitama))

@(require racket/list)
@(require racket/path)

@(define githublink
   {lambda [target]
     (hyperlink target (path->string (file-name-from-path target)))})

@title{@(hyperlink (~url (current-digimon)) (format "~a<sub>~a</sub>" house-garden# cat#))Digital World}
How to construct a @italic{Digital World}? Okay, we don@literal{'}t start with the @italic{File Island}, but we own some concepts from the
@hyperlink["http://en.wikipedia.org/wiki/Digimon"]{Digital Monsters}.

@section{Living Digimons}
@(itemlist #:style 'compact
           (filter-not void? (for/list ([digimon (in-list (directory-list (digimon-world) #:build? #false))])
                               (define homepage (if (string=? (path->string digimon) (digimon-gnome)) (compose1 bold hyperlink) hyperlink))
                               (define info-ref (get-info/full (build-path (digimon-world) digimon))) 
                               (when (procedure? info-ref)
                                 (item (homepage (~url (path->string digimon)) (format "~a: ~a" (info-ref 'collection) (info-ref 'pkg-desc))))))))

@section{Project Conventions}

Since I@literal{'}m an indenpendent developer and communicating costs almost nothing, I make decision at my risk to
model software system in @italic{Formal Methods} and document it with the @italic{Literate Programming} approach.

@subsection{Building Scripts}
@(itemlist #:style 'compact
           @item{@bold{prerequisites.sh}: Build the latest Racket and Swi-Prolog from offical source.}
           @item{@bold{makefile.rkt}: It is the replacement of Makefile, and it@literal{'}s the toplevel one.}
           @item{@bold{submake.rkt}: It is the sub makefile that might exist in every @italic{digimon} directory.})

@subsection{Hierarchy}
@margin-note{The @italic{Digital World} itself as a whole is usually placed within your @litchar{$HOME} directory
 named @litchar{DigitalWorld} if you want to take advantages of the infrastructures.}

For the sake of consistency and better architecture, I follow the concept of @italic{Racket Multi-collection Package}.
Projects/subcollections listed in the root directory are organized as the @italic{digimons}, and each of them may be separated into several repositories.

@(itemlist #:style 'compact
           @nested{@bold{@hyperlink["https://github.com/digital-world/DigiGnome"]{DigiGnome}} is the reserved @italic{digimon} whose duties are making life easy
                    for developers and sharing code base for other @italic{digimons}. Namely @italic{digimon} works like @tt{src}.
                    @(itemlist #:style 'compact
                               @item{@bold{stone} stores immutable meta-information or ancient sources to be translated. Yes, it@literal{'}s the @italic{Rosetta Stone}.}
                               @item{@bold{digitama} is the egg of @italic{digimons}.
                                      Namely sources within it are @bold{hidden} to others. @bold{Compatibility will not be maintained and Use at your own risk!}}
                               @item{@bold{digivice} is the interface for users to talk with @italic{digimons}.
                                      Namely sources within it implement executable tools that will be called by wrappers from @tt{bin}.}
                               @item{@bold{tamer} is the interface for developers to tame the @italic{digimons}. Namely it works like @tt{test}.
                                      After all the @italic{tamer/handbook.scrbl} plays the role of the Test Report along with the Design Documentation
                                      so that the production code could keep elegant.})}
           @nested{@bold{@githublink{/Kuzuhamon}} is another reserved @italic{digimon} who works as the @italic{Per-Tamer Website}
                    if @hyperlink["https://github.com/digital-world/sakuyamon"]{sakuyamon} is deployed in the same machine.
                    @(itemlist #:style 'compact
                               @item{@bold{terminus} manages guilds of @italic{digimons}. Hmm... Sounds weird, nonetheless, try @tt{htdocs}.}
                               @item{@bold{village} stands for @italic{the primary village}, the start point of @italic{digimons}.
                                      Namely sources within it should be translated into proper contents, say @tt{html}, @tt{js}, @tt{css} and so on.})})

@subsection{Version}
Obviousely, our @italic{digimons} have their own life cycle.
@margin-note{The @bold{Baby I} and @bold{Baby II} are merged as the @bold{Baby} to make life simple.}

@(itemlist #:style 'compact
           @item{@bold{Baby}: The 1st stage of @italic{digimon evolution} hatching straightly from her @italic{digitama}. Namely it@literal{'}s the @tt{Alpha Version}.}
           @item{@bold{Child}: The 2nd stage of @italic{digimon evolution} evolving quickly from @bold{Baby}. Namely it@literal{'}s the @tt{Beta Version}.}
           @item{@bold{Adult}: The 3rd stage of @italic{digimon evolution} evolving from @bold{Child}. At this stage @italic{digimons} are strong enough to live on their own.})
