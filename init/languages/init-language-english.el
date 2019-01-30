;;; init-language-english.el --- init for English Language.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ capitalized-words-mode ]

;; (capitalized-words-mode 1)

;;; [ electric punctuation ]

;;; [ stem-english ] -- routines for Stemming English word (Emacs)

(use-package stem-english
  :ensure t
  :defer t)

;;; [ company-english-helper ] -- English helper base on Emacs company-mode.

(use-package company-english-helper
  :quelpa (company-english-helper :fetcher github
                                  :repo "manateelazycat/company-english-helper")
  ;; :load-path "~/Code/Emacs/company-english-helper/"
  :defer t
  :commands (toggle-company-english-helper)
  :config
  (defun my/company-english-helper-setup ()
    (if (local-variable-if-set-p 'company-backends)
        (add-to-list 'company-backends 'company-english-helper-search)
      (add-to-list (make-local-variable 'company-backends) 'company-english-helper-search)))
  (dolist (hook '(org-mode-hook
                  git-commit-mode-hook
                  rcirc-mode-hook))
    (add-hook hook #'my/company-english-helper-setup)))


(provide 'init-language-english)

;;; init-language-english.el ends here
