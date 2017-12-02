;;; init-my-emacs-settings.el --- init my Emacs settings

;;; Commentary:

;;; Code:

;;; [ Disabled Commands ]

;; To enable all disabled commands in one fell swoop, put this in .emacs (not
;; recommended for newbies):
;;
;; uncomment this when you are very familiar with Emacs.

;; (setq disabled-command-function nil)

;;; ----------------------------------------------------------

(fset 'yes-or-no-p 'y-or-n-p) ; treat 'y' as yes, 'n' as no.

(setq inhibit-startup-message t) ; no splash screen

(setq-default initial-scratch-message
              (concat ";; Happy Hacking " (or user-login-name "") "!\n\n"))

;;; [ timer ]

;; (put 'timer-list 'disabled nil)

;; [ Bell ]

(setq visible-bell nil)
;;; disable Emacs built-in bell when [C-g]
(setq ring-bell-function 'ignore)

;;; [ mode ]

;; (setq default-major-mode 'org-mode) ; use org-mode for any unspecified mode.


(provide 'init-my-emacs-settings)

;;; init-my-emacs-settings.el ends here
