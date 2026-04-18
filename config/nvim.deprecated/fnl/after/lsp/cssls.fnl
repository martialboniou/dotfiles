{:filetypes [:css :scss :less]
 :cmd [:vscode-css-language-server :--stdio]
 :root_markers [:package.json :.git]
 :init_options {:provideFormatter true}
 :settings {:css {:validate true}
            :scss {:validate true}
            :less {:validate true}}}
