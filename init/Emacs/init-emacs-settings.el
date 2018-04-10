;;; init-emacs-settings.el --- init my Emacs settings

;;; Commentary:

;;; Code:

(fset 'yes-or-no-p 'y-or-n-p) ; treat 'y' as yes, 'n' as no.
(setq confirm-kill-emacs 'yes-or-no-p)

;; [ Bell ]

(setq visible-bell nil)
;;; disable Emacs built-in bell when [C-g]
(setq ring-bell-function 'ignore)


(provide 'init-emacs-settings)

;;; init-emacs-settings.el ends here
