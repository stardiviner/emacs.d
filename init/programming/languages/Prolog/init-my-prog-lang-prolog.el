;;; init-my-prog-lang-prolog.el --- init Prolog programming languages for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ prolog.el ]

;;; A major mode for Prolog programming under Emacs and offers to my knowledge
;;; the best Prolog IDE out there, especially when used from within XEmacs.

;;; Usage:
;;
;; -

;; (setq load-path (cons "/usr/lib/xemacs/site-lisp" load-path))

(require 'prolog)

(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)

(setq prolog-system 'swi                ; which Prolog are you using? 'swi, 'gnu,
      )

(add-to-list 'auto-mode-alist '("\\.prolog$" . prolog-mode))
;; (add-to-list 'auto-mode-alist '("\\.pl$" . prolog-mode)) ; conflict with Perl extension.
(add-to-list 'auto-mode-alist '("\\.m$" . mercury-mode))




(provide 'init-my-prog-lang-prolog)

;;; init-my-prog-lang-prolog.el ends here
