;;; init-prog-lsp.el --- init for Language Server Protocol

;;; Commentary:



;;; Code:

;;; [ eglot ] -- Client for Language Server Protocol (LSP) servers.

;; (use-package eglot
;;   :hook (prog-mode . eglot-ensure))

;;; [ lsp-mode ] -- clients for servers using Language Server Protocol.

(use-package lsp-mode
  :ensure t
  :defer t
  :load (lsp-clients)
  :commands (lsp lsp-describe-session)
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point))
  :init (setq lsp-auto-configure nil
              lsp-auto-guess-root t
              lsp-prefer-flymake nil) ; use `flycheck' and `lsp-ui'.
  :config
  ;; Support LSP in Org Babel with header argument `:file'.
  ;; https://github.com/emacs-lsp/lsp-mode/issues/377
  (defvar org-babel-lsp-explicit-lang-list
    '("java")
    "Org Mode Babel languages which need explicitly specify header argument :file.")
  (cl-defmacro lsp-org-babel-enbale (lang)
    "Support LANG in org source code block."
    ;; (cl-check-type lang symbolp)
    (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
           (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
      `(progn
         (defun ,intern-pre (info)
           (let ((lsp-file (or (->> info caddr (alist-get :file))
                               buffer-file-name
                               (unless (member ,lang org-babel-lsp-explicit-lang-list)
                                 (concat (org-babel-temp-file (format "lsp-%s-" ,lang))
                                         (cdr (assoc ,lang org-babel-tangle-lang-exts)))))))
             (setq-local buffer-file-name lsp-file)
             (setq-local lsp-buffer-uri (lsp--path-to-uri lsp-file))
             (lsp)))
         (if (fboundp ',edit-pre)
             (advice-add ',edit-pre :after ',intern-pre)
           (progn
             (defun ,edit-pre (info)
               (,intern-pre info))
             (put ',edit-pre 'function-documentation
                  (format "Add LSP info to Org source block dedicated buffer (%s)."
                          (upcase ,lang))))))))

  (defvar org-babel-lsp-lang-list
    '("shell" "python" "ipython" "ruby" "js" "css" "C" "rust" "java" "go"))
  (dolist (lang org-babel-lang-list)
  (dolist (lang org-babel-lsp-lang-list)
    (eval `(lsp-org-babel-enbale ,lang)))

  (defun my/babel-insert-lsp-file-header-argument (lsp-mirror-file)
    "A helper command to insert `:file' header argument path."
    (interactive "fPath to file: ")
    (org-babel-insert-header-arg "file" (format "\"%s\"" lsp-mirror-file)))
  (define-key org-babel-map (kbd "M-f") 'my/babel-insert-lsp-file-header-argument)
  
  ;; load `lsp-clients' for auto configuration of language server clients.
  :config (require 'lsp-clients)
  ;; manage lsp-mode popup buffers
  (add-to-list 'display-buffer-alist
               '("^\\*lsp-help\\*" (display-buffer-below-selected)))
  )

;; [ lsp-ui ] -- UI modules for lsp-mode.

(use-package lsp-ui
  :ensure t
  :defer t
  :after lsp
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions) ; [M-.]
              ([remap xref-find-references] . lsp-ui-peek-find-references)   ; [M-?]
              ("C-c C-j" . lsp-ui-imenu))
  :init (setq lsp-ui-doc-enable nil
              lsp-ui-doc-header nil
              lsp-ui-doc-include-signature t
              lsp-ui-doc-position 'at-point)
  (if (featurep 'xwidget-internal) (setq lsp-ui-doc-use-webkit t))
  ;; (add-to-list 'lsp-ui-doc-frame-parameters )
  )

;; [ company-lsp ] -- company-mode completion backend for lsp-mode.

(use-package company-lsp
  :ensure t
  :defer t
  :after lsp
  :commands company-lsp
  :init
  (defun my:company-lsp-enable ()
    (my-company-add-backend-locally 'company-lsp))
  (add-hook 'lsp-mode-hook #'my:company-lsp-enable))

;;; [ dap-mode ] -- Debug Adapter Protocol mode for lsp-mode.

(use-package dap-mode
  :ensure t
  :defer t
  :after lsp
  :config
  (dap-mode t)
  (dap-ui-mode t))

;;; [ helm-lsp ] -- LSP helm integration.

(use-package helm-lsp
  :ensure t
  :commands (helm-lsp-workspace-symbol helm-lsp-global-workspace-symbol))


(provide 'init-prog-lsp)

;;; init-prog-lsp.el ends here
