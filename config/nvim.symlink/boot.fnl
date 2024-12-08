(import-macros {: tc} :hondana-dev.macros)

(require :hondana-dev/remap)
(require :hondana-dev/set)
(local (_ _) (pcall require :hondana-dev/private))

(macro imports! [...]
  (icollect [_ e (ipairs [...])]
    {:import e}))

(tc type boolean)
(local developer-mode true)

;; set the list of your plugin specs' directories here
(local plugins (imports! :hondana-dev.plugins :hondana-dev.plugins.unchecked))

;; bootstrap lazy
(tc type string)
(local lazy-directory (-> :data (vim.fn.stdpath) (.. :/lazy)))
(let [lazy-path (.. lazy-directory :/lazy.nvim)]
  (when (not (vim.uv.fs_stat lazy-path))
    (vim.fn.system [:git
                    :clone
                    "--filter=blob:none"
                    "https://github.com/folke/lazy.nvim.git"
                    :--single-branch
                    lazy-path]))
  (vim.opt.runtimepath:prepend lazy-path))

(let [{: setup} (require :lazy)
      opts {:checker {:enabled false}
            :defaults {:lazy true}
            :performance {:rtp {:reset false
                                :disabled_plugins [:tutor :matchit :matchparen]}}
            :spec plugins}]
  (setup opts))

;; OPTIONAL (not required but very handy)
;; auto-link fennel.lua from Tangerine to `~/.config/nvim/fnl/fennel.lua`
(when (and developer-mode (-> :Windows (not= _G.jit.os)))
  (tc type string?)
  (local target (-?> :config (vim.fn.stdpath) (.. :/fnl/fennel.lua)))
  (when (and target (= 0 (vim.fn.filereadable target)))
    (let [{:link-tangerine-fennel-lua link} (require :hondana-dev.utils/boot-helper)]
      (link target lazy-directory))))

;; EXPERIMENTAL
;; (-> :config (vim.fn.stdpath) (.. :/experimental)
;;     (#(when (vim.uv.fs_stat $) (vim.opt.rtp:append $))))
;; (fn _G.onDictChanged []
;;   (print :foo))

;; force Sleuth to be verbose so a 'sleuth' buffer variable
;; can be watched using `dictwatcheradd()`
;; (vim.cmd "cnoreabbrev <expr> Sleuth getcmdtype() == ':' && getcmdline() ==# 'Sleuth' ? 'verbose Sleuth' : 'Sleuth'")
;; (vim.cmd "let s:sleuth_reset=0
;; function! OnSleuthChanged(d,k,z)
;;   let specialft='lua'
;;   echom \"checked!!\"
;;   if has_key(a:z,'old')
;;     if a:z['old']['filetype'] == &specialft
;;       let s:sleuth_reset=1
;;     endif
;;   elseif s:sleuth_reset==1 && has_key(a:z,'new')
;;     if a:z['new']['filetype'] == &specialft
;;       echomsg 'change stop to' a:z['new']['options']['shiftwidth'][0]
;;       let s:sleuth_reset=0
;;     endif
;;   endif
;; endfunction")
;;
;; (let [{:nvim_create_autocmd au :nvim_create_augroup augroup} vim.api
;;       callback #(do
;;                   (print "testing...")
;;                   (vim.cmd "silent! cal dictwatcherdel(b:, 'sleuth', 'OnSleuthChanged')")
;;                   (vim.cmd "cal dictwatcheradd(b:, 'sleuth', 'OnSleuthChanged')"))
;;       pattern ["*.lua" "*.fnl"]]
;;   ;; NOPE!
;;   (au [:BufRead :BufNewFile :BufEnter] {: callback : pattern}))

;; v:lua.onDictChanged()
