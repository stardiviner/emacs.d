;;; init-prog-lsp.el --- init for Language Server Protocol

;;; Commentary:



;;; Code:

;;; [ lsp-mode ] -- clients for servers using Language Server Protocol.

(use-package lsp-mode
  :ensure t
  :commands lsp
  :load (lsp-clients) ; load `lsp-clients' for auto configuration of language server clients.
  :init (setq lsp-auto-guess-root t))

;; [ lsp-ui ] -- UI modules for lsp-mode.

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :init (setq lsp-ui-doc-enable nil
              lsp-ui-doc-header t
              lsp-ui-doc-include-signature t
              ;; lsp-ui-sideline-update-mode 'point
              lsp-ui-doc-position 'at-point))

;; [ company-lsp ] -- company-mode completion backend for lsp-mode.

(use-package company-lsp
  :after company
  :ensure t
  :commands company-lsp
  :init (setq company-lsp-enable-recompletion t)
  :config
  (defun my:company-lsp-enable ()
    (my-company-add-backend-locally 'company-lsp))
  (add-hook 'lsp-mode-hook #'my:company-lsp-enable))

;;; [ dap-mode ] -- Debug Adapter Protocol mode for lsp-mode.

(use-package dap-mode
  :ensure t
  :after lsp)


(provide 'init-prog-lsp)

;;; init-prog-lsp.el ends here
