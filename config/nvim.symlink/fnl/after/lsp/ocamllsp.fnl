(local {: api} vim)

(local language-id-of {:menhir :ocaml.menhir
                       :ocamlinterface :ocaml.interface
                       :ocamllex :ocaml.ocamllex})

(fn get_language_id [_ ftype]
  ;; if no key found, return ftype
  (->> ftype (. language-id-of) (#(if $ $ ftype))))

(fn root_dir [bufnr on-dir]
  (let [U (require :hondana-dev.utils.root-pattern)
        fname (api.nvim_buf_get_name bufnr)]
    (on-dir ((U.root-pattern "*.opam" :esy.json :package.json :.git
                             :dune-project :dune-workspace) fname))))

{:filetypes [:ocaml :menhir :ocamlinterface :ocamllex :reason :dune]
 :cmd [:ocamllsp]
 : root_dir
 : get_language_id}
