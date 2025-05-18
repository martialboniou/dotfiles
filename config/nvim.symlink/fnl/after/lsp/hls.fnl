;; UNUSED EXCEPT IF YOU DISABLED `haskell-tools.nvim`
{:filetypes [:haskell]
 :cmd [:haskell-language-server-wrapper :--lsp]
 :root_markers ["*.cabal" :stack.yaml :cabal.project :package.yaml :hie.yaml]
 :settings {:haskell {:formattingProvider "ormolu"
                      :cabalFormattingProvider :cabalfmt}}}
