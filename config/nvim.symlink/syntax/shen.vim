" Vim syntax file
" Language:     Shen
" Maintainer:   Matúš Kmiť <nimaai@runbox.com>
" URL:          http://github.com/nimaai/vim-shen.git
" Description:  Syntax definition for Shen language (www.shenlanguage.org)

setlocal lisp
setlocal lispwords-=let
setlocal lispwords-=if
setlocal lispwords-=do
setlocal lispwords+=defprolog
setlocal lispwords+=datatype
setlocal lispwords+=package

setlocal iskeyword+=$,+,-,*,/,.,>,<,=,:,!,',&
setlocal iskeyword+=@-@

" system functions
syntax keyword shenSyntax <-address
syntax keyword shenSyntax <-vector
syntax keyword shenSyntax abort
syntax keyword shenSyntax abs
syntax keyword shenSyntax absvector
syntax keyword shenSyntax absvector?
syntax keyword shenSyntax address->
syntax keyword shenSyntax adjoin
syntax keyword shenSyntax alpha?
syntax keyword shenSyntax alphanum?
syntax keyword shenSyntax and
syntax keyword shenSyntax append
syntax keyword shenSyntax append-files
syntax keyword shenSyntax append-files-with-open-stream
syntax keyword shenSyntax approx
syntax keyword shenSyntax arity
syntax keyword shenSyntax ascii
syntax keyword shenSyntax asserta
syntax keyword shenSyntax assertz
syntax keyword shenSyntax assoc
syntax keyword shenSyntax assoc-if
syntax keyword shenSyntax assoc-if-not
syntax keyword shenSyntax assocp
syntax keyword shenSyntax assocp-if
syntax keyword shenSyntax assocp-if-not
syntax keyword shenSyntax atom?
syntax keyword shenSyntax bag=?
" next unsure
syntax keyword shenSyntax bar!
syntax keyword shenSyntax bind
syntax keyword shenSyntax boolean
syntax keyword shenSyntax boolean?
syntax keyword shenSyntax bootstrap
syntax keyword shenSyntax bound?
syntax keyword shenSyntax call
syntax keyword shenSyntax cartprod
syntax keyword shenSyntax cartprodp
syntax keyword shenSyntax cases
syntax keyword shenSyntax cd
syntax keyword shenSyntax ceiling
syntax keyword shenSyntax close
syntax keyword shenSyntax cn
syntax keyword shenSyntax compile
syntax keyword shenSyntax compress
syntax keyword shenSyntax concat
syntax keyword shenSyntax concat*
syntax keyword shenSyntax cond
syntax keyword shenSyntax cons
syntax keyword shenSyntax cons?
syntax keyword shenSyntax converge
syntax keyword shenSyntax copy-file
syntax keyword shenSyntax copy-file-with-open-stream
syntax keyword shenSyntax cos
syntax keyword shenSyntax cos135
syntax keyword shenSyntax cos150
syntax keyword shenSyntax cos210
syntax keyword shenSyntax cos225
syntax keyword shenSyntax cos30
syntax keyword shenSyntax cos315
syntax keyword shenSyntax cos330
syntax keyword shenSyntax cos45
syntax keyword shenSyntax cosh
syntax keyword shenSyntax coth
syntax keyword shenSyntax count
syntax keyword shenSyntax count-if
syntax keyword shenSyntax csch
syntax keyword shenSyntax cube
syntax keyword shenSyntax declare
syntax keyword shenSyntax dense?
syntax keyword shenSyntax depopulate
syntax keyword shenSyntax destroy
syntax keyword shenSyntax difference
syntax keyword shenSyntax digit?
syntax keyword shenSyntax div
syntax keyword shenSyntax do
syntax keyword shenSyntax drop
syntax keyword shenSyntax drop-last
syntax keyword shenSyntax e
syntax keyword shenSyntax element?
syntax keyword shenSyntax empty?
syntax keyword shenSyntax enable-type-theory
syntax keyword shenSyntax error
syntax keyword shenSyntax error-to-string
syntax keyword shenSyntax eval
syntax keyword shenSyntax eval-kl
syntax keyword shenSyntax even?
syntax keyword shenSyntax every?
syntax keyword shenSyntax exception
syntax keyword shenSyntax explode
syntax keyword shenSyntax expt
syntax keyword shenSyntax external
syntax keyword shenSyntax factorial
syntax keyword shenSyntax factorise
syntax keyword shenSyntax factorise?
syntax keyword shenSyntax fail
syntax keyword shenSyntax fail-if
syntax keyword shenSyntax file
syntax keyword shenSyntax file-exists?
syntax keyword shenSyntax file-extension
syntax keyword shenSyntax file-size
syntax keyword shenSyntax filter
syntax keyword shenSyntax find
syntax keyword shenSyntax findall
syntax keyword shenSyntax fix
syntax keyword shenSyntax floor
syntax keyword shenSyntax fn
syntax keyword shenSyntax foldl
syntax keyword shenSyntax foldr
syntax keyword shenSyntax for
syntax keyword shenSyntax foreign
syntax keyword shenSyntax fork
syntax keyword shenSyntax freeze
syntax keyword shenSyntax fresh
syntax keyword shenSyntax fst
" function is deprecated; use fn instead
syntax keyword shenSyntax function
syntax keyword shenSyntax g
syntax keyword shenSyntax gcd
syntax keyword shenSyntax gensym
syntax keyword shenSyntax get
syntax keyword shenSyntax get-time
syntax keyword shenSyntax hash
syntax keyword shenSyntax hd
syntax keyword shenSyntax hdstr
syntax keyword shenSyntax hdv
syntax keyword shenSyntax head
syntax keyword shenSyntax hush?
syntax keyword shenSyntax if
syntax keyword shenSyntax implementation
syntax keyword shenSyntax in
syntax keyword shenSyntax in-package
syntax keyword shenSyntax include
syntax keyword shenSyntax include-all-but
syntax keyword shenSyntax included
syntax keyword shenSyntax indentation
syntax keyword shenSyntax index
syntax keyword shenSyntax index-last
syntax keyword shenSyntax inferences
syntax keyword shenSyntax infix?
syntax keyword shenSyntax input
syntax keyword shenSyntax input+
syntax keyword shenSyntax insert
syntax keyword shenSyntax integer?
syntax keyword shenSyntax intern
syntax keyword shenSyntax internal
syntax keyword shenSyntax intersection
syntax keyword shenSyntax is
syntax keyword shenSyntax is!
syntax keyword shenSyntax isqrt
syntax keyword shenSyntax it
syntax keyword shenSyntax lambda
syntax keyword shenSyntax language
syntax keyword shenSyntax lazy
syntax keyword shenSyntax lcd
syntax keyword shenSyntax lcm
syntax keyword shenSyntax length
syntax keyword shenSyntax let
syntax keyword shenSyntax limit
syntax keyword shenSyntax lineread
syntax keyword shenSyntax list
syntax keyword shenSyntax list->string
syntax keyword shenSyntax list->vector
syntax keyword shenSyntax load
syntax keyword shenSyntax loaded
syntax keyword shenSyntax log
syntax keyword shenSyntax log10
syntax keyword shenSyntax log2
syntax keyword shenSyntax loge
syntax keyword shenSyntax lowercase
syntax keyword shenSyntax lowercase?
syntax keyword shenSyntax macroexpand
syntax keyword shenSyntax make-string
syntax keyword shenSyntax map
syntax keyword shenSyntax mapc
syntax keyword shenSyntax mapcan
syntax keyword shenSyntax mapf
syntax keyword shenSyntax maths.lazyfor-and
syntax keyword shenSyntax maths.lazyfor-or
syntax keyword shenSyntax max
syntax keyword shenSyntax maxinferences
syntax keyword shenSyntax min
syntax keyword shenSyntax mod
syntax keyword shenSyntax modf
syntax keyword shenSyntax n->string
syntax keyword shenSyntax n-times
syntax keyword shenSyntax natural?
syntax keyword shenSyntax negative?
syntax keyword shenSyntax newv
syntax keyword shenSyntax nl
syntax keyword shenSyntax not
syntax keyword shenSyntax nth
syntax keyword shenSyntax nthrt
syntax keyword shenSyntax null
syntax keyword shenSyntax number
syntax keyword shenSyntax number?
syntax keyword shenSyntax occurrences
syntax keyword shenSyntax occurs-check
syntax keyword shenSyntax occurs?
syntax keyword shenSyntax odd?
syntax keyword shenSyntax open
syntax keyword shenSyntax optimise
syntax keyword shenSyntax optimise?
syntax keyword shenSyntax or
syntax keyword shenSyntax os
syntax keyword shenSyntax out
syntax keyword shenSyntax output
syntax keyword shenSyntax package
syntax keyword shenSyntax package?
syntax keyword shenSyntax pairoff
syntax keyword shenSyntax partition
syntax keyword shenSyntax permute
syntax keyword shenSyntax pi
syntax keyword shenSyntax populate
syntax keyword shenSyntax populated?
syntax keyword shenSyntax porous?
syntax keyword shenSyntax port
syntax keyword shenSyntax porters
syntax keyword shenSyntax pos
syntax keyword shenSyntax positive?
syntax keyword shenSyntax power
syntax keyword shenSyntax powerset
syntax keyword shenSyntax pprint
syntax keyword shenSyntax pps
syntax keyword shenSyntax pr
syntax keyword shenSyntax preclude
syntax keyword shenSyntax preclude-all-but
syntax keyword shenSyntax prefix?
syntax keyword shenSyntax pretty-string
syntax keyword shenSyntax prime?
syntax keyword shenSyntax print
syntax keyword shenSyntax product
syntax keyword shenSyntax profile
syntax keyword shenSyntax profile-results
syntax keyword shenSyntax prolog-memory
syntax keyword shenSyntax prolog?
syntax keyword shenSyntax protect
syntax keyword shenSyntax ps
syntax keyword shenSyntax put
syntax keyword shenSyntax radians
syntax keyword shenSyntax random
syntax keyword shenSyntax read
syntax keyword shenSyntax read-byte
syntax keyword shenSyntax read-file
syntax keyword shenSyntax read-file-as-bytelist
syntax keyword shenSyntax read-file-as-string
syntax keyword shenSyntax read-from-string
syntax keyword shenSyntax read-from-string-unprocessed
syntax keyword shenSyntax receive
syntax keyword shenSyntax release
syntax keyword shenSyntax remove
syntax keyword shenSyntax remove-duplicates
syntax keyword shenSyntax remove-if
syntax keyword shenSyntax render
syntax keyword shenSyntax render-file
syntax keyword shenSyntax reopen
syntax keyword shenSyntax reseed
syntax keyword shenSyntax retract
syntax keyword shenSyntax return
syntax keyword shenSyntax reverse
syntax keyword shenSyntax round
syntax keyword shenSyntax run
syntax keyword shenSyntax s-op1
syntax keyword shenSyntax s-op2
syntax keyword shenSyntax save
syntax keyword shenSyntax sech
syntax keyword shenSyntax series
syntax keyword shenSyntax set
syntax keyword shenSyntax set-indentation
syntax keyword shenSyntax set-linelength
syntax keyword shenSyntax set-tolerance
syntax keyword shenSyntax set=?
syntax keyword shenSyntax set?
syntax keyword shenSyntax simple-error
syntax keyword shenSyntax sin
syntax keyword shenSyntax sin120
syntax keyword shenSyntax sin135
syntax keyword shenSyntax sin225
syntax keyword shenSyntax sin240
syntax keyword shenSyntax sin300
syntax keyword shenSyntax sin315
syntax keyword shenSyntax sin45
syntax keyword shenSyntax sinh
syntax keyword shenSyntax snd
syntax keyword shenSyntax some?
syntax keyword shenSyntax sort
syntax keyword shenSyntax sparse?
syntax keyword shenSyntax specialise
syntax keyword shenSyntax spell-number
syntax keyword shenSyntax splice
syntax keyword shenSyntax spy
syntax keyword shenSyntax spy?
syntax keyword shenSyntax sq
syntax keyword shenSyntax sqrt
syntax keyword shenSyntax sqrt2
syntax keyword shenSyntax step
syntax keyword shenSyntax step?
syntax keyword shenSyntax stinput
syntax keyword shenSyntax stoutput
syntax keyword shenSyntax str
syntax keyword shenSyntax stream
syntax keyword shenSyntax string
syntax keyword shenSyntax string->list
syntax keyword shenSyntax string->n
syntax keyword shenSyntax string->symbol
syntax keyword shenSyntax string.count
syntax keyword shenSyntax string.difference
syntax keyword shenSyntax string.element?
syntax keyword shenSyntax string.every?
syntax keyword shenSyntax string.infix?
syntax keyword shenSyntax string.intersection
syntax keyword shenSyntax string.length
syntax keyword shenSyntax string.map
syntax keyword shenSyntax string.nth
syntax keyword shenSyntax string.prefix?
syntax keyword shenSyntax string.reverse
syntax keyword shenSyntax string.set=?
syntax keyword shenSyntax string.set?
syntax keyword shenSyntax string.some?
syntax keyword shenSyntax string.subset?
syntax keyword shenSyntax string.suffix?
syntax keyword shenSyntax string.trim
syntax keyword shenSyntax string.trim-if
syntax keyword shenSyntax string.trim-left
syntax keyword shenSyntax string.trim-left-if
syntax keyword shenSyntax string.trim-right
syntax keyword shenSyntax string.trim-right-if
syntax keyword shenSyntax string<=?
syntax keyword shenSyntax string<?
syntax keyword shenSyntax string>=?
syntax keyword shenSyntax string>?
syntax keyword shenSyntax string?
syntax keyword shenSyntax strip-extension
syntax keyword shenSyntax subbag?
syntax keyword shenSyntax subset?
syntax keyword shenSyntax subst
syntax keyword shenSyntax suffix?
syntax keyword shenSyntax sum
syntax keyword shenSyntax summation
syntax keyword shenSyntax symbol
syntax keyword shenSyntax symbol?
syntax keyword shenSyntax synonyms
syntax keyword shenSyntax system-S?
syntax keyword shenSyntax systemf
syntax keyword shenSyntax tail
syntax keyword shenSyntax take
syntax keyword shenSyntax take-last
syntax keyword shenSyntax tan
syntax keyword shenSyntax tan120
syntax keyword shenSyntax tan150
syntax keyword shenSyntax tan210
syntax keyword shenSyntax tan240
syntax keyword shenSyntax tan30
syntax keyword shenSyntax tan300
syntax keyword shenSyntax tan330
syntax keyword shenSyntax tan60
syntax keyword shenSyntax tanh
syntax keyword shenSyntax tc
syntax keyword shenSyntax tc?
syntax keyword shenSyntax thaw
syntax keyword shenSyntax time
syntax keyword shenSyntax tl
syntax keyword shenSyntax tlstr
syntax keyword shenSyntax tlv
syntax keyword shenSyntax tokenise
syntax keyword shenSyntax tolerance
syntax keyword shenSyntax track
syntax keyword shenSyntax tracked
syntax keyword shenSyntax transitive-closure
syntax keyword shenSyntax trap-error
syntax keyword shenSyntax trim
syntax keyword shenSyntax trim-if
syntax keyword shenSyntax trim-left
syntax keyword shenSyntax trim-left-if
syntax keyword shenSyntax trim-right
syntax keyword shenSyntax trim-right-if
syntax keyword shenSyntax tuple?
syntax keyword shenSyntax type
syntax keyword shenSyntax u!
syntax keyword shenSyntax undefmacro
syntax keyword shenSyntax union
syntax keyword shenSyntax unit
syntax keyword shenSyntax unix
syntax keyword shenSyntax unprofile
syntax keyword shenSyntax unput
syntax keyword shenSyntax unspecialise
syntax keyword shenSyntax untrack
syntax keyword shenSyntax update-lambda-table
syntax keyword shenSyntax uppercase
syntax keyword shenSyntax uppercase?
syntax keyword shenSyntax userdefs
syntax keyword shenSyntax v-op1
syntax keyword shenSyntax v-op2
syntax keyword shenSyntax vacant?
syntax keyword shenSyntax value
syntax keyword shenSyntax var?
syntax keyword shenSyntax variable?
syntax keyword shenSyntax vector
syntax keyword shenSyntax vector->
syntax keyword shenSyntax vector->list
syntax keyword shenSyntax vector.append
syntax keyword shenSyntax vector.dfilter
syntax keyword shenSyntax vector.dmap
syntax keyword shenSyntax vector.element?
syntax keyword shenSyntax vector.every?
syntax keyword shenSyntax vector.map
syntax keyword shenSyntax vector.reverse
syntax keyword shenSyntax vector.some?
syntax keyword shenSyntax vector?
syntax keyword shenSyntax verified
syntax keyword shenSyntax version
syntax keyword shenSyntax when
syntax keyword shenSyntax where
syntax keyword shenSyntax whitespace?
syntax keyword shenSyntax write-byte
syntax keyword shenSyntax write-to-file
syntax keyword shenSyntax x->ascii
syntax keyword shenSyntax y-or-n?

syntax keyword shenSyntax @p
syntax keyword shenSyntax @s
syntax keyword shenSyntax @v
syntax keyword shenSyntax $
syntax keyword shenSyntax +
syntax keyword shenSyntax -
syntax keyword shenSyntax *
syntax keyword shenSyntax /
syntax keyword shenSyntax /.
syntax keyword shenSyntax >
syntax keyword shenSyntax <
syntax keyword shenSyntax =
syntax keyword shenSyntax ==
syntax keyword shenSyntax >=
syntax keyword shenSyntax <=

syntax keyword shenDef define
syntax keyword shenDef defmacro
syntax keyword shenDef defprolog
syntax keyword shenDef defcc
syntax keyword shenDef defun
" next unsure
syntax keyword shenDef datatypes

syntax keyword shenBoolean true
syntax keyword shenBoolean false

" Shen-YACC
syntax keyword shenYACCSyntax compile

syntax match shenYACCNonTerminal "<!>"
syntax match shenYACCNonTerminal "<e>"
syntax match shenYACCNonTerminal "<end>"
syntax match shenYACCNonTerminal "<\k\+>"

" Shen special characters
syntax keyword shenSpecial <-
syntax keyword shenSpecial <--
syntax keyword shenSpecial :=
syntax match shenSpecial ";"
syntax keyword shenSpecial ->
syntax keyword shenSpecial where
syntax region shenSpecial start="*" end="*"
" TODO: test the following ones
syntax keyword shenSpecial ==>
syntax keyword shenSpecial >>
" syntax keyword shenSpecial _
" next unsure
syntax keyword shenSpecial mode

syntax match shenList "]"
syntax match shenList "\["
syntax match shenList "|"
syntax match shenNumber "\<\d\+\(\.\=\d\+\)\=\>"
syntax match shenVariable "\<\u\k*\>"
syntax match shenComment "\\\\.*$"
syntax match shenFunctionName "(\<\k*\>\s\{-}"hs=s+1 contains=shenDef,shenSyntax
syntax region shenComment start="\\\*" end="\*\\"
syntax region shenString start=,", end=,",
syntax region shenFunctionType start="{" end="}"

syntax keyword shenGlobals *home-directory*
syntax keyword shenGlobals *hush*
syntax keyword shenGlobals *implementation*
syntax keyword shenGlobals *language*
syntax keyword shenGlobals *macros*
syntax keyword shenGlobals *maximum-print-sequence-size*
syntax keyword shenGlobals *os*
syntax keyword shenGlobals *port*
syntax keyword shenGlobals *porters*
syntax keyword shenGlobals *property-vector*
syntax keyword shenGlobals *release*
syntax keyword shenGlobals *stinput*
syntax keyword shenGlobals *stoutput*
syntax keyword shenGlobals *version*

command -nargs=+ HiLink hi def link <args>

HiLink shenSyntax Function
HiLink shenDef Define
HiLink shenVariable Identifier
HiLink shenSpecial Special
HiLink shenNumber Number
HiLink shenComment Comment
HiLink shenString String
HiLink shenBoolean Boolean
HiLink shenFunctionType Typedef
HiLink shenYACCSyntax Statement
HiLink shenYACCNonTerminal Constant
HiLink shenList Structure
HiLink shenFunctionName Ignore
HiLink shenGlobals Constant

delcommand HiLink

" highlight SpecialChar cterm=bold ctermfg=1
" highlight Special cterm=bold term=bold gui=bold
" highlight Statement cterm=bold term=bold gui=bold
highlight Ignore cterm=bold term=bold gui=bold

let b:current_syntax = "shen"
