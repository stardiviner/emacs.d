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




;;; ----------------------------------------------------------------------------

(provide 'init-my-prog-language-server-protocol)

;;; init-my-prog-language-server-protocol.el ends here
