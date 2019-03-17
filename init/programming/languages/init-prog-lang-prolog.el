;;; init-prog-lang-prolog.el --- init Prolog programming languages for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ prolog ]

(use-package prolog
  :ensure t
  :defer t
  :mode (("\\.prolog\\'" . prolog-mode) ("\\.m\\'" . mercury-mode))
  :commands (run-prolog proglog-mode mercury-mode)
  ;; which Prolog are you using? 'swi, 'gnu,
  :init (setq prolog-system 'swi))


;;; [ ediprolog ] -- Emacs Does Interactive Prolog

(use-package ediprolog
  :ensure t
  :defer t
  :bind (:map prolog-mode-map ([f10] . ediprolog-dwim)))

;; [ ob-prolog ] -- babel for Prolog

(use-package ob-prolog
  :ensure t
  :defer t
  :commands (org-babel-execute:prolog)
  :config
  (add-to-list 'org-babel-load-languages '(prolog . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))


(provide 'init-prog-lang-prolog)

;;; init-prog-lang-prolog.el ends here
