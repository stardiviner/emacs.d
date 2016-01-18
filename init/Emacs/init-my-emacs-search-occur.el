;;; init-my-emacs-search-occur.el --- init for occur
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ occur ]

(define-key my-search-prefix (kbd "o") 'occur)


;;; [ multi-occur ]

(define-key my-search-prefix (kbd "O") 'multi-occur)

(define-key my-search-prefix (kbd "M-o") 'multi-occur-in-matching-buffers)

(define-key my-highlight-symbol-prefix (kbd "M-r") 'highlight-lines-matching-regexp)
(define-key my-search-prefix (kbd "M-h") 'how-many)


;;; [ swoop ] -- Peculiar buffer navigation for Emacs.

(use-package swoop
  ;; :ensure t
  :config
  (global-set-key (kbd "C-o") 'swoop)
  (global-set-key (kbd "C-M-o") 'swoop-multi)
  (global-set-key (kbd "M-o")   'swoop-pcre-regexp)
  (global-set-key (kbd "C-S-o") 'swoop-back-to-last-position)
  )


(provide 'init-my-emacs-search-occur)

;;; init-my-emacs-search-occur.el ends here
