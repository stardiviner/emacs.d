;;; init-my-emacs-shell.el --- init Shell in Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; variables

;; "$SHELL", "/bin/sh", "/bin/bash", "usr/bin/zsh"
;; (setq shell-file-name (getenv "SHELL"))


;;; [ Shell ]

;; M-x shell is a nice shell interface to use, let's make it colorful. If
;; you need a terminal emulator rather than just a shell, consider M-x term
;; instead.
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; (setq shell-command-completion-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'init-eshell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init-my-emacs-shell)

;;; init-my-emacs-shell.el ends here
