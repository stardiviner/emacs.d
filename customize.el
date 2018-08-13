(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(custom-safe-themes)
 '(eyebrowse-keymap-prefix "w")
 '(fci-rule-color "#dedede")
 '(global-hl-line-mode nil)
 '(hl-sexp-background-color "#efebe9")
 '(org-trello-current-prefix-keybinding "C-c o")
 '(rg-keymap-prefix nil)
 '(safe-local-variable-values
   (quote
    ((org-src-preserve-indentation)
     (eval and
           (featurep
            (quote ox-extra))
           (ox-extras-activate
            (quote
             (ignore-headlines))))
     (eval require
           (quote ox-texinfo+)
           nil t)
     (eval require
           (quote ox-extra)
           nil t)
     (eval require
           (quote org-man)
           nil t)
     (eval require
           (quote magit-utils)
           nil t)
     (firestarter . value)
     (major-mode . lua-mode)
     (elisp-lint-indent-specs
      (if-let* . 2)
      (when-let* . 1)
      (let* . defun)
      (nrepl-dbind-response . 2)
      (cider-save-marker . 1)
      (cider-propertize-region . 1)
      (cider--make-result-overlay . 1)
      (multiline-comment-handler . defun)
      (insert-label . defun)
      (insert-align-label . defun)
      (insert-rect . defun)
      (cl-defun . 2)
      (with-parsed-tramp-file-name . 2)
      (thread-first . 1)
      (thread-last . 1))
     (checkdoc-package-keywords-flag)
     (org-html-infojs-options)
     (org-startup-with-inline-images)
     (org-indent-mode)
     ))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
