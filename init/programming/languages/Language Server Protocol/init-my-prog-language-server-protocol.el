;;; init-my-prog-language-server-protocol.el --- init for Language Server Protocol

;;; Commentary:



;;; Code:

;;; [ lsp-mode ] -- A Emacs Lisp library for implementing clients for servers using Microsoft's Language Server Protocol.

(use-package lsp-mode
  :ensure t
  :config
  ;; (lsp-mode t)
  (add-hook 'prog-mode-hook #'lsp-mode)
  (with-eval-after-load 'lsp-mode
    (require 'lsp-flycheck))

  (use-package company-lsp
    :ensure t
    :config
    (push 'company-lsp company-backends)
    (setq company-lsp-cache-candidates t)
    )
  )


(use-package lsp-java
  :ensure t)

;;; [ lsp-javacomp ] -- Emacs Language Server client backed by JavaComp.

(use-package lsp-javacomp
  :ensure t
  :init
  ;; Make sure lsp-javacomp is loaded when lsp-mode is used so that the JavaComp
  ;; LSP handler is installed.
  (with-eval-after-load 'lsp-mode
    (require 'lsp-javacomp))
  ;; Enable `lsp-mode' for `java-mode'.
  (add-hook 'java-mode-hook 'lsp-mode)
  :config
  ;; (setq lsp-javacomp-server-jar "path/to/JavaComp.jar")
  )

(use-package lsp-go
  :ensure t)

(use-package lsp-rust
  :ensure t)

(use-package lsp-python
  :ensure t)

(use-package lsp-haskell
  :ensure t)


(provide 'init-my-prog-language-server-protocol)

;;; init-my-prog-language-server-protocol.el ends here
