;;; PACKS EL-GET
;;
(require 'noaccess)

(add-to-list 'load-path (joindirs mars/local-root-dir "el-get" "el-get"))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously ; MUST USE BEFORE 'FLIM REQUIREMENT
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(unless (boundp 'el-get-recipe-path)
 (error "el-get is not installed"))

;;; MY PERSONAL RECIPE DIRECTORY
;;
(add-to-list 'el-get-recipe-path (joindirs mars/local-root-dir mars/personal-data "Recipes"))

;;; MY INSTALL WISHES
;;
;; NOTE: use standard functions in :AFTER to be able to make a recipe from the code
(setq el-get-sources '((:name evil
                              :build `(("make" "all" ,(when (executable-find "texi2pdf") "info")))
                              :features nil)
                       (:name evil-leader ; installs evil & undo-tree
                              :features nil)
                       (:name evil-surround
                              :features nil
                              :post-init nil)
                       (:name evil-numbers
                              :features nil) ; let 'VIM-EVERYWHERE configure 'EVIL
                       (:name buffer-move
                              :post-init nil)
                       (:name autopair
                              :features nil)
                       (:name multi-term
                              :features nil)
                       (:name yasnippet
                              :features nil) ; let 'RECTIFY configure 'YASNIPPET
                       (:name ace-jump-mode
                              :after (progn
                                       (global-set-key (kbd "C-c SPC") #'ace-jump-mode)
                                       (define-key global-map (kbd "C-x SPC") #'ace-jump-mode-pop-mark)
                                       (eval-after-load "evil-mode"
                                         '(define-key evil-normal-state-map
                                            (kbd "SPC") #'ace-jump-mode))
                                       (eval-after-load "viper"
                                         '(define-key viper-vi-global-user-map
                                            (kbd "SPC") #'ace-jump-mode)))
                              :features nil) ; let 'BEHAVE configure 'ACE-JUMP-MODE
                       (:name highlight-parentheses
                              :features nil) ; HIGHLIGHT-PARENTHESES-MODE is autoloaded
                       (:name chopper
                              :post-init nil) ; use personal keybindings windows & buffers chops
                       (:name bbdb            ; BBDB for wanderlust
                              :branch "v2.x"
                              :build `("autoconf" ,(concat "./configure --with-emacs=" el-get-emacs)
                                       "make clean" "rm -f lisp/bbdb-autoloads.el"
                                       "make autoloadsc info")
                              :features bbdb-autoloads
                              :info "texinfo")
                       (:name haskell-mode
                              :build (("make")))
                       (:name emms      ; installs emacs-w3m
                              :info nil ; TODO: 2013-05-23 fix info file
                              :build `(("make" ,(format "EMACS=%s" el-get-emacs)
                                        ,(format "SITEFLAG=\\\"--no-site-file -L %s/emacs-w3m/ \\\"" el-get-dir)
                                        "autoloads" "lisp"
                                        ,(when (executable-find "taglib-config") "emms-print-metadata")))
                              :features nil) ; let 'MEDIA configure 'EMMS
                       (:name git-emacs
                              :features nil)
                                        ;                       (:name nxhtml
                                        ;                              :load nil)
                       )) ; choose 'NXHTML or 'MULTI-WEB-MODE in 'WEB-PROGRAMMING
;; :name ropemacs :build '(("python" "setup.py" "install" "||" "sudo" "python" "setup.py" "install"))
;; latex case
(condition-case nil
    (progn
      ;; FIXME: ConTeXt and other case
      (el-get-executable-find "latex")
      (add-to-list 'el-get-sources '(:name auctex
                                           :branch "release_11_87"
                                           :build `(("./autogen.sh")
                                                    ("./configure"
                                                     ,(if (or (string= "" mars/texmf-dir)
                                                              (null (file-accessible-directory-p mars/texmf-dir)))
                                                          "--without-texmf-dir"
                                                        (concat "--with-texmf-dir=" (expand-file-name mars/texmf-dir)))
                                                     "--with-lispdir=`pwd`"
                                                     ,(concat "--with-emacs=" el-get-emacs))
                                                    "make lisp docs"
                                                    "cd preview && make lisp && cd ..")
                                           :load nil))) ; let 'WIKI-WIKI launch 'TEX-SITE
  (error (message "packs-el-get: AUCTeX won't be automatically installed without TeXLive or any modern LaTeX distribution.")))
;; ruby case
(condition-case nil ;; TODO: ruby compilation case
    (progn
      ;; TODO: find a way to install RINARI correctly
      (el-get-executable-find "rake")
      nil
      ;; YASNIPPET install is ugly (requires python *and* ruby)
      ;; check pygments module is installed
      ;; (add-to-list 'el-get-sources '(:name yasnippet
      ;;                                      :build '("rake compile")))
      ;;(add-to-list 'el-get-sources '(:name rinari
      ;;                                     :build '(("git" "submodule" "init")
      ;;                                              ("git" "submodule" "update")
      ;;                                              ))) ; doc:install_info OBSOLETE b/c no ginstall-info revision
                                        ;(add-to-list 'el-get-sources '(:name inf-ruby-bond
                                        ;                                     :depends nil)
                                        ; ) ; use RINARI's INF-RUBY
      ;; TODO: restore ruby-electric or create a new rcp
      ;; (setq mars/el-get-packages (nconc mars/el-get-packages '(ruby-electric)))
      )
  (error (message "packs-el-get: yasnippet won't be compiled and rinari and other packages for ruby won't be installed without rake, a simple ruby build program.")))

;;; MY EL-GET PACKAGE
;;
(defvar mars/el-get-packages nil
  "List of highly recommended el-get recipes.")

;;; IMPORTANT: cl-lib is required if this Emacs is a pre-24.3 release
(let ((cl-lib-uninstalled (null (locate-library "cl-lib"))))
  (if (not *i-want-full-ammo*)
      (setq mars/el-get-packages (when cl-lib-uninstalled '(cl-lib))) ; no package unless `*i-want-full-ammo*'
    (progn
      (setq mars/el-get-packages '(el-get
                                   linum-off
                                   linum-relative
                                   linum+
                                   hl-line+
                                   escreen
                                   keats
                                   shen-mode
                                   ;; haskellmode-emacs
                                   pylookup
                                   pymacs
                                   mailcrypt
                                   auto-complete
                                   ac-slime
                                   redshank
                                   smex
                                   anything
                                   bookmark+
                                   auto-pair-plus
                                   mouse-embrace ; use C-c e to toggle; C-S-mouse-3 to configure; mouse-3 to paste surrounding a selection
                                   dired-details
                                   dired-plus
                                   org-mode
                                   ;; howm ; corrupted tar
                                   ;; remember
                                   multi-web-mode
                                   rainbow-mode
                                   markdown-mode
                                   magit
                                   ;; darcsum
                                   header2
                                   filladapt
                                   ack-and-a-half
                                   pp-c-l
                                   hideshowvis
                                   gist
                                   switch-window
                                   cycle-buffer
                                   js-comint
                                   textile-mode
                                   yaml-mode
                                   haml-mode
                                   hexview-mode ; my own recipe to autoload it
                                   sunrise-commander
                                   wanderlust
                                   ;; from .emacs.d/data/Recipes
                                   revive-plus
                                   mwe-log-commands))
      (when cl-lib-uninstalled (add-to-list 'mars/el-get-packages 'cl-lib))
      (when (< emacs-major-version 24) (add-to-list 'mars/el-get-packages 'color-theme))
      ;; merge EL-GET-SOURCE with MARS/EL-GET-PACKAGE
      (setq mars/el-get-packages
            (nconc mars/el-get-packages
                   (mapcar #'el-get-source-name el-get-sources))))))

(el-get 'sync mars/el-get-packages)
(el-get 'wait)

(provide 'packs-el-get)                 ; required by 'ADAPTER
