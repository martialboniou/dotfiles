(local {: api} vim)

(local language-id-of {:menhir :ocaml.menhir
                       :ocamlinterface :ocaml.interface
                       :ocamllex :ocaml.ocamllex})

(local root_dir (let [{: root-dir} (require :hondana-dev.utils.fns)]
                  (root-dir "*.opam" :esy.json :package.json :.git
                            :dune-project :dune-workspace)))

(fn get_language_id [_ ftype]
  ;; if no key found, return ftype
  (->> ftype (. language-id-of) (#(if $ $ ftype))))

{:filetypes [:ocaml :menhir :ocamlinterface :ocamllex :reason :dune]
 :cmd [:ocamllsp]
 : root_dir
 : get_language_id}
