(import-macros {: tc} :hondana-dev.macros)

(local builder #{:cmd "zig"
                 :name "zig compiler errors"
                 :args [:build "-freference-trace"]
                 :components [{1 :on_output_parse
                               :parser {:diagnostics [:extract
                                                      "^(.*):(%d+):(%d+):%s*(%a+):%s*(.*)$"
                                                      :filename
                                                      :lnum
                                                      :col
                                                      :type
                                                      :text
                                                      :code]}}
                              ;; TODO:
                              ;;; check doc.rendering.md after refactoring
                              ;; {1 :display_duration :detail_level 2}
                              ;; {1 :on_output_summarize :max_lines 4}
                              ;; {1 :on_exit_set_status :success_codes []}
                              ;; {1 :on_complete_dispose
                              ;;  :timeout 300
                              ;;  :statuses ["SUCCESS" "FAILURE" "CANCELED"]
                              ;;  :require_view ["SUCCESS" "FAILURE"]}
                              {1 :on_result_diagnostics
                               :remove_on_restart true}
                              {1 :restart_on_save :delay 1}
                              [:default]]})

(tc type "overseer.TemplateFileDefinition")
(local T {:name "Zig" : builder :condition {:filetype :zig}})

T
