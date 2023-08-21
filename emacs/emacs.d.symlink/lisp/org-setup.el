(defvar evil-auto-indent nil)
(declare-function org-indent-mode (&optional arg))

;;; PACKAGES

(hondana/use org
  :config
  (setq org-ellipsis " ▾")
  (defun hondana/org-mode-setup ()
    (org-indent-mode)
    (visual-line-mode 1))
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 ()
                                  (compose-region
                                   (match-beginning 1)
                                   (match-end 1) "•")))))))
(add-hook 'org-mode-hook 'hondana/org-mode-setup)

(hondana/use org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(hondana/use visual-fill-column
  :straight (visual-fill-column
             :host codeberg
             :repo "joostkremers/visual-fill-column")
  :config
  (defun hondana/visual-fill ()
    (setq-default visual-fill-column-width 83
                  visual-fill-column-center-text t)
    (visual-fill-column-mode 1))
  (add-hook 'visual-line-mode-hook 'hondana/visual-fill))


; ;; Make sure org-indent face is available
; (require 'org-indent)

(provide 'org-setup)
