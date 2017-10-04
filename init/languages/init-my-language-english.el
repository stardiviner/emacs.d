;;; init-my-language-english.el --- init for English Language.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ capitalized-words-mode ]

;; (capitalized-words-mode 1)


;;; [ captain ] -- CAPiTalization is Automatic IN emacs.

(use-package captain
  :ensure t
  :commands (captain-capitalize-word)
  :config
  (global-captain-mode 1)
  )

;;; [ electric punctuation ]

;;; [ stem ] -- stemming library for Emacs

(use-package stem
  :ensure t)

;;; [ stem-english ] -- routines for Stemming English word (Emacs)

(use-package stem-english
  :ensure t
  :config
  (defun my-stem-english (word)
    "My smart apply of `stem-english'."
    (let ((stem-word (stem-english word)))
      (if (= 1 (length stem-word))
          word
        (nth 1 stem-word))))
  )


(provide 'init-my-language-english)

;;; init-my-language-english.el ends here
