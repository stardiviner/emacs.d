;;; init-my-emacs-completion.el --- my Emacs completion frameworks init

;;; Commentary:


;;; Code:

;;; [ complete ]

;; (setq tab-always-indent 'complete)
;; (add-to-list 'completion-styles 'initials t)
;; (setq completion-cycle-threshold 5)
(setq completion-at-point-functions 'auto-complete)


;;; [ pcomplete ] --- Programmable, Context-Sensitive Completion Library

(load-library "pcomplete")


;;; [ Icomplete ]

;; (icomplete-mode 1)



;;; Press [TAB] in minibuffer to show completions in popup window buffer.



(require 'init-ido)
(require 'init-helm)
(require 'init-auto-complete)
(require 'init-company-mode)



(provide 'init-my-emacs-completion)

;;; init-my-emacs-completion.el ends here
