;;; init-my-emacs-enhancement.el --- Emacs Enhancements
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ describe-number ] -- Describe number value at point in Emacs.

;; - "b->d" means binary to decimal.
;; - "o->d" means octal to decimal.
;; - "x->d" means hexadecimal to decimal.

(use-package describe-number
  :config
  (define-key my-prog-help-document-map (kbd "N") 'describe-number-at-point)
  )



(provide 'init-my-emacs-enhancement)

;;; init-my-emacs-enhancement.el ends here
