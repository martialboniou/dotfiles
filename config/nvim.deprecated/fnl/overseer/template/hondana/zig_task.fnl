(import-macros {: tc} :hondana-dev.macros)

(local parser #(let [(filename line-number col-number type text code) ;
                     ($:match "^(.*):(%d+):(%d+):%s*(%a+):%s*(.*)$")
                     lnum (tonumber line-number)]
                 (when lnum
                   {:col (tonumber col-number)
                    : filename
                    : lnum
                    : text
                    : type
                    : code})))

(local builder #{:cmd "zig"
                 :name "zig compiler errors"
                 :args [:build "-freference-trace"]
                 :components [{1 :on_output_parse : parser}
                              {1 :on_exit_set_status :success_codes []}
                              {1 :on_complete_dispose
                               :timeout 300
                               :statuses ["SUCCESS" "FAILURE" "CANCELED"]
                               :require_view ["SUCCESS" "FAILURE"]}
                              {1 :on_result_diagnostics
                               :remove_on_restart true}
                              {1 :restart_on_save :delay 1}]})

(tc type "overseer.TemplateFileDefinition")
(local T {:name "ZigTask" : builder :condition {:filetype :zig}})

T
