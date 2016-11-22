;;; init-my-prog-lang-prolog.el --- init Prolog programming languages for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ prolog.el ]

;;; A major mode for Prolog programming under Emacs and offers to my knowledge
;;; the best Prolog IDE out there, especially when used from within XEmacs.

(use-package prolog
  :init
  (autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
  (autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
  (autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)

  (add-to-list 'auto-mode-alist '("\\.prolog\\'" . prolog-mode))
  (add-to-list 'auto-mode-alist '("\\.m\\'" . mercury-mode))
  :config
  ;; which Prolog are you using? 'swi, 'gnu,
  (setq prolog-system 'swi)
  )


;;; [ ediprolog ] -- Emacs Does Interactive Prolog

(use-package ediprolog
  :ensure t
  :bind (:map prolog-mode-map
              ([f10] . ediprolog-dwim))
  )

;; [ ob-prolog ] -- babel for Prolog

(use-package ob-prolog
  :ensure t)


(provide 'init-my-prog-lang-prolog)

;;; init-my-prog-lang-prolog.el ends here
