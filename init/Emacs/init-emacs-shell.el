;;; init-emacs-shell.el --- init Shell in Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Shell ] -- [M-x shell]

(use-package shell
  :defer t
  :commands (shell)
  :init
  ;; [M-x shell] is a nice shell interface to use, let's make it colorful.
  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  ;; If you need a terminal emulator rather than just a shell, consider [M-x term]
  ;; instead.
  :config
  ;; for auto nifty command substitution [!!] and ^a^b.
  (define-key shell-mode-map (kbd "SPC") 'comint-magic-space)

  (add-to-list 'display-buffer-alist ; open shell buffer in current window
               '("^\\*shell\\*$" . (display-buffer-below-selected)))

  ;; (add-to-list 'display-buffer-alist
  ;;              '("^\\*Shell Command Output\\*$" . (display-buffer-reuse-window display-buffer-below-selected)))
  ;; (add-to-list 'display-buffer-alist
  ;;              '("^\\*Async Shell Command\\*" (display-buffer-reuse-window display-buffer-below-selected)))
  
  ;;; a helper function to run sudo Shell command.
  (defun sudo-shell-command (command)
    "The suggested way to run sudo Shell `COMMAND' with TRAMP's sudo method."
    (interactive "MAsync sudo command (root): ")
    (with-temp-buffer
      (cd "/sudo::/")
      (async-shell-command command)))
  )

;;; [ Eshell ]

(require 'init-eshell)



(provide 'init-emacs-shell)

;;; init-emacs-shell.el ends here
