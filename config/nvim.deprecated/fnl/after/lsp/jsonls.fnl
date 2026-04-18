;; b0o/schemastore.nvim plugin must be installed
{:filetypes [:json :jsonc]
 :cmd [:vscode-json-language-server :--stdio]
 :root_markers [:.git]
 :settings {:json {:validate {:enable true}
                   :schemas (-> :schemastore (require) (#($.json.schemas)))}}}
