" Tomorrow Night palette based on:
" - the https://doc.rust-lang.org/book css theme
" - adapted from https://github.com/jmblog/color-themes-for-highlightjs
" - originally created by https://github.com/chriskempson/tomorrow-theme
" #c5c8c6 -- tomorrow foreground
" #1d1f21 -- tomorrow background
" *selection: #373b41 (unused in book)
" *line: #282a2e (unused in book)
" *window: #4d5057 (unused in book)
" #969896 -- tomorrow comment (comment)
" #cc6666 -- tomorrow variable_attribute_tag_regexp_red (variable, attribute, tag, regexp, ruby-constant, xml-tag-title, xml-doctype, xml-pi, html-doctype, css-id, css-class, css-pseudo)
" #de935f -- tomorrow params_constant_number_preprocessor_pragma_builtin_literals_orange (params, constant, number, preprocessor, pragma, built-in, literal)
" #f0c674 -- tomorrow yellow (ruby-class-title, css-rule-attribute)
" #b5bd68 -- tomorrow string_value_inheritance_header_name_green (string, value, inheritance, header, name, ruby-symbol, xml-cdata)
" #8abeb7 -- tomorrow title_aqua (title, css-hexcolor)
" #81a2be -- tomorrow blue (python-decoration+title, ruby-function-title+title-keyword)
" #b294bb -- tomorrow keyword_function_purple (hljs-keyword, hljs-function)
" #718c00 -- addition (only used in book)
" #c82829 -- deletion (only used in book)
" WORKING IN PROGRESS
hi clear
if exists("syntax_on")
  syntax reset
endif
let colors_name = "hondana-tomorrow-night"

if &background ==# 'dark'
  let s:window = '#4d5057' " (unused in book)
  let s:gray20PercGrayer = '#45525E' " untouched

  let s:dimBackground = '#1d1f21' " AKA tomorrow background
  let s:dimBackground2 = '#1d1f21' " AKA tomorrow background
  let s:normal ='#c5c8c6' " AKA tomorrow foreground
  let s:comment = '#969896' " AKA  tomorrow comment
  let s:keyword_function_purple = '#b294bb'
  let s:string_value_inheritance_header_name_green = '#b5bd68' " ruby-symbol, xml-cdata
  let s:variable_attribute_tag_regexp_red = '#cc6666' " ruby-constant, xml-tag-title, xml-doctype, xml-pi, html-doctype, css-id, css-class, css-pseudo)
  let s:title_aqua = '#8abeb' " css-hexcolor
  let s:params_constant_number_preprocessor_pragma_builtin_literals_orange = '#de935f'
  let s:yellow = '#f0c674' " ruby-class-title, css-rule-attribute

  exe "hi! @property guifg="      . s:variable_attribute_tag_regexp_red . " guibg=NONE gui=NONE"
  exe "hi! @string guifg="      . s:string_value_inheritance_header_name_green . " guibg=NONE gui=NONE"
  exe "hi! @number guifg="      . s:params_constant_number_preprocessor_pragma_builtin_literals_orange . " guibg=NONE gui=NONE"
  exe "hi! @keyword guifg="      . s:keyword_function_purple . " guibg=NONE gui=NONE"
  exe "hi! @include guifg="      . s:keyword_function_purple . " guibg=NONE gui=NONE"
  exe "hi! @variable guifg="      . s:normal . " guibg=NONE gui=NONE"
  exe "hi! @keyword.function guifg="      . s:keyword_function_purple . " guibg=NONE gui=NONE"
  exe "hi! @keyword.operator guifg="      . s:keyword_function_purple . " guibg=NONE gui=NONE"
  exe "hi! @constant.builtin guifg="      . s:variable_attribute_tag_regexp_red . " guibg=NONE gui=NONE"
  exe "hi! @tag guifg="      . s:variable_attribute_tag_regexp_red . " guibg=NONE gui=NONE"
  exe "hi! @tag.delimiter guifg="      . s:variable_attribute_tag_regexp_red . " guibg=NONE gui=NONE"
  exe "hi! @label guifg="      . s:variable_attribute_tag_regexp_red . " guibg=NONE gui=NONE"
  exe "hi! @punctuation.bracket guifg="      . s:normal . " guibg=NONE gui=NONE"
  exe "hi! @function guifg="      . s:title_aqua . " guibg=NONE gui=NONE"
  exe "hi! @method guifg="      . s:title_aqua . " guibg=NONE gui=NONE"
  exe "hi! @function.call guifg="      . s:normal . " guibg=NONE gui=NONE"
  exe "hi! @method.call guifg="      . s:normal . " guibg=NONE gui=NONE"
  exe "hi! @punctuation.special guifg="      . s:normal . " guibg=NONE gui=NONE"
  exe "hi! @punctuation.delimiter guifg="      . s:normal . " guibg=NONE gui=NONE"
  exe "hi! @parameter guifg=". s:normal . " guibg=NONE gui=NONE"
  exe "hi! Normal guifg=" . s:normal . " guibg=" . s:dimBackground . " gui=NONE"
  exe "hi! @constructor guifg=". s:title_aqua . " guibg=NONE gui=NONE"
  exe "hi! Type guifg=". s:title_aqua . " guibg=NONE gui=NONE"
  exe "hi! @type guifg=". s:normal . " guibg=NONE gui=NONE"
  exe "hi! @type.builtin guifg=". s:normal . " guibg=NONE gui=NONE"
  exe "hi! @type.definition guifg=". s:normal . " guibg=NONE gui=NONE"
  exe "hi! @operator guifg=". s:variable_attribute_tag_regexp_red . " guibg=NONE gui=NONE"
  exe "hi! @boolean guifg=". s:variable_attribute_tag_regexp_red . " guibg=NONE gui=NONE"
  exe "hi! StatusLine guifg=" . s:dimBackground2 . " guibg=" . s:window . " gui=NONE"
  exe "hi! StatusLineNC guifg=" . s:normal . " guibg=" . s:dimBackground2 . " gui=NONE"
  exe "hi! Identifier guifg="      . s:title_aqua . " guibg=NONE gui=NONE"

  exe "hi! CursorLineNr guifg=". s:window . " guibg=NONE gui=NONE"
  exe "hi! LineNr guifg=". s:window . " guibg=NONE gui=NONE"
  exe "hi! LineNrAbove guifg=". s:gray20PercGrayer . " guibg=NONE gui=NONE"
  exe "hi! LineNrBelow guifg=". s:gray20PercGrayer . " guibg=NONE gui=NONE"

  exe "hi! ColorColumn guifg=NONE guibg=". s:dimBackground2 . "  gui=NONE"

  exe "hi! CursorLine guifg=NONE guibg=". s:dimBackground2 . "  gui=NONE"
  exe "hi! Search guifg=NONE " . " guibg=" . s:yellow . " gui=NONE"
  exe "hi! Pmenu guifg=" . s:normal . " guibg=" . s:dimBackground2 . " gui=NONE"
  exe "hi! PmenuSel guifg=" . s:dimBackground2 . " guibg=" . s:normal . " gui=NONE"
  exe "hi! Comment guifg=". s:comment . " guibg=NONE gui=NONE"
  exe "hi! SignColumn guifg=" . s:window . " guibg=" . s:dimBackground . " gui=NONE"
  exe "hi! @symbol guifg=". s:string_value_inheritance_header_name_green . " guibg=NONE gui=NONE"
  exe "hi! @float guifg=". s:variable_attribute_tag_regexp_red . " guibg=NONE gui=NONE"
  exe "hi! Label guifg=". s:variable_attribute_tag_regexp_red . " guibg=NONE gui=NONE"
  exe "hi! @field guifg=". s:variable_attribute_tag_regexp_red . " guibg=NONE gui=NONE"
  exe "hi! @namespace guifg=". s:normal . " guibg=NONE gui=NONE"
  exe "hi! @variable.builtin guifg=" . s:normal . " guibg=NONE gui=NONE"
  exe "hi! @conditional guifg="      . s:keyword_function_purple . " guibg=NONE gui=NONE"
  exe "hi! TabLine guifg=" . s:window . " guibg=" . s:dimBackground2 . " gui=NONE"
  exe "hi! TabLineFill guifg=NONE " . " guibg=" . s:dimBackground . " gui=NONE"
  exe "hi! TabLineSel guifg=" . s:normal . " guibg=" . s:dimBackground . " gui=NONE"
  exe "hi! Title guifg=" . s:normal . " guibg=NONE gui=bold"
  exe "hi! Constant guifg="      . s:params_constant_number_preprocessor_pragma_builtin_literals_orange . " guibg=NONE gui=NONE"
  exe "hi! @todo guifg=" . s:window . " guibg=NONE gui=NONE"
  exe "hi! @repeat guifg=" . s:keyword_function_purple . " guibg=NONE gui=NONE"
  exe "hi! Special guifg=" . s:title_aqua . " guibg=NONE gui=NONE"
  exe "hi! Exception guifg=" . s:keyword_function_purple . " guibg=NONE gui=NONE"
  exe "hi! Visual guifg=" . s:dimBackground . " guibg=" . s:window . " gui=NONE"
  exe "hi! MatchParen guifg=NONE guibg=" . s:yellow . " gui=bold"
  exe "hi! PreProc guifg=" . s:title_aqua . " guibg=NONE gui=NONE"
  exe "hi! NonText guifg=" . s:gray20PercGrayer . " guibg=NONE gui=NONE"

  " -- diff
  let s:diffGreen = '#255028'
  let s:diffRed = '#792421'
  let s:diffText = '#9b711e'

  exe "hi! DiffAdd guifg=NONE " . " guibg=" . s:diffGreen . " gui=NONE"
  exe "hi! DiffDelete guifg=NONE " . " guibg=" . s:diffRed . " gui=NONE"
  exe "hi! DiffChange guifg=NONE " . " guibg=" . s:yellow . " gui=NONE"
  exe "hi! DiffText guifg=NONE guibg=" . s:diffText . " gui=NONE"
  "
  " -- netwr
  exe "hi! Directory guifg=" . s:string_value_inheritance_header_name_green . " guibg=NONE gui=NONE"

  " lsp"
  exe "hi! DiagnosticError  guifg=". s:diffRed . " guibg=NONE gui=NONE"
  "
  " javascript
  exe "hi! @constructor.javascript  guifg=". s:params_constant_number_preprocessor_pragma_builtin_literals_orange . " guibg=NONE gui=NONE"

  " yaml
  exe "hi! @field.yaml guifg=". s:variable_attribute_tag_regexp_red . " guibg=NONE gui=NONE"

  " lua
  exe "hi! @operator.lua guifg=". s:keyword_function_purple . " guibg=NONE gui=NONE"
  exe "hi! @field.lua guifg=". s:normal . " guibg=NONE gui=NONE"
  exe "hi! @function.call.lua guifg=". s:variable_attribute_tag_regexp_red . " guibg=NONE gui=NONE"
  exe "hi! link @function.builtin.lua @function.call.lua"

  exe "hi! NoiceCmdlinePopupBorder  guifg=" . s:normal . " guibg=NONE gui=NONE"

  exe "hi! OilDir  guifg=" . s:variable_attribute_tag_regexp_red . " guibg=NONE gui=NONE"
endif

hi! link @punctuation.special.markdown @text.title.markdown
hi! link Statement @text.title.markdown
hi! link FoldColumn SignColumn
hi! link SpecialKey NonText
hi! link Folded NonText
hi! link NormalFloat Normal
hi! TODO guifg=NONE guibg=NONE gui=bold
hi! VertSplit guifg=NONE guibg=NONE gui=NONE
hi! EndOfBuffer guifg=NONE guibg=NONE gui=NONE
hi! @text.emphasis guifg=NONE guibg=NONE gui=italic
hi! @text.strong guifg=NONE guibg=NONE gui=bold

" TS
"  hi! link TSFunction @function
exe "hi! TSFunction  guifg="      . s:title_aqua . " guibg=NONE gui=NONE"
exe "hi! TSFunctionCall guifg="      . s:normal . " guibg=NONE gui=NONE"

" Disable lsp semantic tokens
hi! link @lsp.type.namespace @namespace
hi! link @lsp.type.type @type
hi! link @lsp.type.class @type
hi! link @lsp.type.enum @type
hi! link @lsp.type.interface @type
hi! link @lsp.type.struct @structure
hi! link @lsp.type.parameter @parameter
hi! link @lsp.type.variable @variable
hi! link @lsp.type.property @property
hi! link @lsp.type.enumMember @constant
hi! link @lsp.type.function @function
hi! link @lsp.type.method @method
hi! link @lsp.type.macro @macro
hi! link @lsp.type.decorator @function

hi! link @lsp.type.type.terraform @keyword

hi! netrwMarkFile gui=bold

hi! LspSignatureActiveParameter gui=bold,italic,underline

" Third party plugins
hi! EyelinerPrimary gui=bold
hi! EyelinerSecondary gui=italic
hi! link QuickScopePrimary EyelinerPrimary
hi! link QuickScopeSecondary EyelinerSecondary

hi! link NoiceCmdlinePopupBorderSearch NoiceCmdlinePopupBorder
hi! link NoiceCmdlinePopupTitle NoiceCmdlinePopupBorder
hi! link NoiceCmdlineIcon NoiceCmdlinePopupBorder
hi! link NoiceCmdlineIconSearch NoiceCmdlinePopupBorder
hi! link NoiceVirtualText NonText

" hi! link LspLens NonText

hi! link CopilotSuggestion NonText


