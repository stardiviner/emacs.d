;;; init-emacs-enhancement.el --- Emacs Enhancements
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ describe-number ] -- Describe number value at point in Emacs.

;; - "b->d" means binary to decimal.
;; - "o->d" means octal to decimal.
;; - "x->d" means hexadecimal to decimal.

(use-package describe-number
  :config
  (define-key document-prefix (kbd "N") 'describe-number-at-point)
  )



(provide 'init-emacs-enhancement)

;;; init-emacs-enhancement.el ends here
