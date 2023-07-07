! Copyright (C) 2011 Martial Boniou.
! See http://factorcode.org/license.txt for BSD license.
! hack from editors.emacs by Slava Pestov.
USING: combinators.short-circuit editors kernel make math.parser
namespaces sequences system vocabs ;
IN: editors.emacs-mars

SINGLETON: emacsclient-mars
emacsclient-mars editor-class set-global

SYMBOL: emacsclient-path
SYMBOL: emacsserver-name

HOOK: default-emacsclient os ( -- path )
HOOK: default-emacsserver os ( -- path )

M: object default-emacsclient ( -- path ) "emacsclient" ;
M: object default-emacsserver ( -- path ) "mars"        ;

M: emacsclient-mars editor-command ( file line -- command )
    [
        {
            [ emacsclient-path get-global ]
            [ default-emacsclient dup emacsclient-path set-global ]
        } 0|| ,
        {
            [ emacsserver-name get-global ]
            [ default-emacsserver dup emacsserver-name set-global ]
        } 0|| "--socket-name=" prepend ,
        "--no-wait" ,
        number>string "+" prepend ,
        ,
    ] { } make ;

os windows? [ "editors.emacs-mars.windows" require ] when
