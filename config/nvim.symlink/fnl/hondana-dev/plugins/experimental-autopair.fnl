;;; AUTOPAIRS
;; maintenance mode
;; testing with zig
;; should not be applied in lisp as kovisoft/paredit is always ON
{1 :altermo/ultimate-autopair.nvim
 :event [:InsertEnter :CmdlineEnter]
 :branch "v0.6"
 :opts {:extensions {:filetype {:nft [:TelescopePrompt]}}}}
