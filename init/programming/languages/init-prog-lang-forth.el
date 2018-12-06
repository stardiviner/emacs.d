;;; init-prog-lang-forth.el --- init for Forth Programming Language.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
;;; [ forth-mode ] -- Programming language mode for Forth.

(use-package forth-mode
  :ensure t
  :config
  (defun my-func/open-and-switch-to-buffer (the-command the-buffer-name &optional whether-switch-to-buffer)
    "Open a `COMMAND', and switch to that `BUFFER' depend on `OPTION'.

Usage: 

 (define-key Org-prefix (kbd 'o')
   (lambda ()
     (interactive)
     (my-func/open-and-switch-to-buffer 'org-agenda-list \"*Org Agenda*\" t)))
"
    (interactive)
    (if (get-buffer the-buffer-name)
        (switch-to-buffer the-buffer-name)
      (funcall the-command)
      (bury-buffer)
      (when whether-switch-to-buffer
        (switch-to-buffer the-buffer-name))))
  
  (defun my-forth-switch-to-repl ()
    "Switch to Forth REPL buffer."
    (interactive)
    (my-func/open-and-switch-to-buffer 'forth-interaction-mode "*forth*"))
  
  (define-key forth-mode-map (kbd "C-c C-s") 'my-forth-switch-to-repl)
  )

;;; ----------------------------------------------------------------------------

(provide 'init-prog-lang-forth)

;;; init-prog-lang-forth.el ends here
