;;; init-prog-lsp.el --- init for Language Server Protocol

;;; Commentary:



;;; Code:

;;; [ lsp-mode ] -- clients for servers using Language Server Protocol.

(use-package lsp-mode
  :ensure t
  :init (setq lsp-enable-flycheck nil
              lsp-enable-indentation nil
              lsp-highlight-symbol-at-point nil
              lsp-eldoc-render-all nil
              ;; inhibit lsp progress message in echo area.
              lsp-inhibit-message t
              lsp-message-project-root-warning t)
  ;; (add-hook 'prog-mode-hook #'lsp-mode) ; XXX: use in lang-specific mode hook.
  :config
  ;; Restart server/workspace in case the lsp server exits unexpectedly.
  ;; https://emacs-china.org/t/topic/6392
  (defun lsp-restart-server ()
    "Restart LSP server."
    (interactive)
    (lsp-restart-workspace)
    (revert-buffer t t)
    (message "LSP server restarted."))

  ;; Support LSP in org babel
  ;; https://github.com/emacs-lsp/lsp-mode/issues/377
  (cl-defmacro lsp-org-babel-enbale (lang &optional enable-name)
    "Support LANG in org source code block. "
    (cl-check-type lang string)
    (cl-check-type enable-name (or null string))
    (let ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
          (client (intern (format "lsp-%s-enable" (or enable-name lang)))))
      `(progn
         (defun ,edit-pre (babel-info)
           (let ((lsp-file (or (->> babel-info caddr (alist-get :file))
                               buffer-file-name)))
             (setq-local buffer-file-name lsp-file)
             (setq-local lsp-buffer-uri (lsp--path-to-uri lsp-file))
             (,client)))
         (put ',edit-pre 'function-documentation
              (format "Prepare local buffer environment for org source block (%s)."
                      (upcase ,lang))))))

  ;; FIXME: Project detection
  ;; If nil, use the current directory
  ;; https://github.com/emacs-lsp/lsp-python/issues/28
  (defun my-default-directory ()
    "Returns the current directory."
    default-directory)
  (advice-add #'lsp--suggest-project-root :after-until #'my-default-directory)

  (require 'lsp-imenu)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu))

;; [ lsp-ui ] -- UI modules for lsp-mode.

(use-package lsp-ui
  :ensure t
  :defer t
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :hook (lsp-mode . lsp-ui-mode)
  ;; :init (add-hook 'lsp-after-open-hook #'lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-doc-header t
        lsp-ui-doc-include-signature t
        ;; lsp-ui-doc-position 'at-point
        lsp-ui-sideline-update-mode 'point))

;; [ company-lsp ] -- company-mode completion backend for lsp-mode.

(use-package company-lsp
  :after company
  :ensure t
  :init (setq company-lsp-enable-recompletion t
              company-lsp-enable-snippet t
              company-lsp-cache-candidates t
              company-lsp-async t)
  :config
  (defun my:company-lsp-enable ()
    (my-company-add-backend-locally 'company-lsp))
  (add-hook 'lsp-mode-hook #'my:company-lsp-enable))


(provide 'init-prog-lsp)

;;; init-prog-lsp.el ends here
