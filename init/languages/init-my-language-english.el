;;; init-my-language-english.el --- init for English Language.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ capitalized-words-mode ]

;; (capitalized-words-mode 1)


;;; [ captain ] -- CAPiTalization is Automatic IN emacs.

(use-package captain
  :ensure t
  :defer t
  :commands (captain-capitalize-word)
  :init
  (global-captain-mode 1))

;;; [ electric punctuation ]

;;; [ stem-english ] -- routines for Stemming English word (Emacs)

(use-package stem-english
  :ensure t
  :defer t)


(provide 'init-my-language-english)

;;; init-my-language-english.el ends here
