;; a friendly programming language from the future: https://www.unison-lang.org
(local root_dir (let [{: root-dir} (require :hondana-dev.utils.fns)]
                  (root-dir "*.u")))

{:filetype [:unison]
 :cmd [:nc :localhost (or (os.getenv :UNISON_LSP_PORT) :5757)]
 : root_dir
 :settings {}}
