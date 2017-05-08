;;; init-my-prog-language-server-protocol.el --- init for Language Server Protocol

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
;;; [ lsp-mode ] -- A Emacs Lisp library for implementing clients for servers using Microsoft's Language Server Protocol.

(use-package lsp-mode
  :ensure t
  :config
  (global-lsp-mode t)
  (with-eval-after-load 'lsp-mode
    (require 'lsp-flycheck)))


(use-package lsp-java
  :ensure t)

(use-package lsp-go
  :ensure t)

(use-package lsp-rust
  :ensure t)

(use-package lsp-python
  :ensure t)

(use-package lsp-haskell
  :ensure t)


;;; ----------------------------------------------------------------------------

(provide 'init-my-prog-language-server-protocol)

;;; init-my-prog-language-server-protocol.el ends here
