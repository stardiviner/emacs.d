;;; init-prog-lsp.el --- init for Language Server Protocol

;;; Commentary:



;;; Code:

;;; [ lsp-mode ] -- A Emacs Lisp library for implementing clients for servers using Microsoft's Language Server Protocol.

(use-package lsp-mode
  :ensure t
  :defer t
  :preface (setq lsp-enable-flycheck nil
                 lsp-enable-indentation nil
                 lsp-highlight-symbol-at-point nil)
  :init (add-hook 'prog-mode-hook #'lsp-mode)
  :config
  ;; [ lsp-ui ] -- UI modules for lsp-mode.
  (use-package lsp-ui
    :ensure t
    :init (add-hook 'lsp-after-open-hook #'lsp-ui-mode))
  ;; [ company-lsp ] -- company-mode completion backend for lsp-mode.
  (use-package company-lsp
    :ensure t
    :init (push 'company-lsp company-backends)
    :config
    (setq company-lsp-enable-recompletion t
          company-lsp-enable-snippet t
          company-lsp-cache-candidates t
          company-lsp-async t)
    )
  )


(use-package lsp-go
  :ensure t)

(use-package lsp-rust
  :ensure t)

(use-package lsp-python
  :ensure t)

(use-package lsp-haskell
  :ensure t)


(provide 'init-prog-lsp)

;;; init-prog-lsp.el ends here
