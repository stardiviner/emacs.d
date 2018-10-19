;;; init-prog-lsp.el --- init for Language Server Protocol

;;; Commentary:



;;; Code:

;;; [ lsp-mode ] -- A Emacs Lisp library for implementing clients for servers using Microsoft's Language Server Protocol.

;; use `lsp-{language}-enable' instead.
(use-package lsp-mode
  :ensure t
  :init (setq lsp-enable-flycheck nil
              lsp-enable-indentation nil
              lsp-highlight-symbol-at-point nil
              lsp-eldoc-render-all nil
              ;; inhibit lsp progress message in echo area.
              lsp-inhibit-message t)
  ;; :init (add-hook 'prog-mode-hook #'lsp-mode) ; XXX: use in lang-specific mode hook.
  ;; :config
  ;; auto set lsp workspace to `projectile-project-root'.
  ;; (defun my:set-lsp-workspace ()
  ;;   (when projectile-project-root
  ;;     ;; FIXME:
  ;;     (setq lsp--cur-workspace projectile-project-root)))
  ;; (add-hook 'lsp-before-open-hook #'my:set-lsp-workspace)
  )

;; [ lsp-ui ] -- UI modules for lsp-mode.

(use-package lsp-ui
  :ensure t
  :defer t
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-doc-header t
        lsp-ui-doc-include-signature t
        ;; lsp-ui-doc-position 'at-point
        lsp-ui-sideline-update-mode 'point
        )
  (add-hook 'lsp-after-open-hook #'lsp-ui-mode)

  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  )

;; [ company-lsp ] -- company-mode completion backend for lsp-mode.

(use-package company-lsp
  :ensure t
  :init
  (setq company-lsp-enable-recompletion t
        company-lsp-enable-snippet t
        company-lsp-cache-candidates t
        company-lsp-async t)

  (defun my:company-lsp-enable ()
    (my-company-add-backend-locally 'company-lsp))
  (add-hook 'lsp-mode-hook #'my:company-lsp-enable))


(provide 'init-prog-lsp)

;;; init-prog-lsp.el ends here
