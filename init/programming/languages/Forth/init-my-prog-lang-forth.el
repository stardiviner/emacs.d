;;; init-my-prog-lang-forth.el --- init for Forth Programming Language.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
;;; [ forth-mode ] -- Programming language mode for Forth.

(use-package forth-mode
  :ensure t
  :config
  (defun my-forth-switch-to-repl ()
    "Switch to Forth REPL buffer."
    (interactive)
    (my-func/open-and-switch-to-buffer 'forth-interaction-mode "*forth*"))
  
  (define-key forth-mode-map (kbd "C-c C-s") 'my-forth-switch-to-repl)
  )

;;; ----------------------------------------------------------------------------

(provide 'init-my-prog-lang-forth)

;;; init-my-prog-lang-forth.el ends here
