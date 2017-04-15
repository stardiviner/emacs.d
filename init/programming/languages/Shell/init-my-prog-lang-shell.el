;;; init-my-prog-lang-shell.el --- init for Shell Scripts
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ `sh-mode' (shell-script-mode ]

(setq sh-indentation 2
      sh-basic-offset 2)

;;; [ Bash ]

;;; [ Zsh ]

(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

;; A programmatic way of selecting a flavor when you don't want to use the
;; shebang is doing this in a sh-mode buffer:
(defun my-sh-mode-zsh-setup ()
  "Setup `sh-mode' engine to Zsh."
  (interactive)
  (if (and (buffer-file-name) ; filer out non-file buffers which will returns nil
           (string-match "\\.zsh$" (buffer-file-name)))
      (sh-set-shell "zsh")))

(add-hook 'sh-mode-hook #'my-sh-mode-zsh-setup)


(provide 'init-my-prog-lang-shell)

;;; init-my-prog-lang-shell.el ends here
