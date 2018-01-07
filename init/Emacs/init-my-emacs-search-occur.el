;;; init-my-emacs-search-occur.el --- init for occur
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ occur ]

(define-key search-prefix (kbd "o") 'occur)

(add-to-list 'display-buffer-alist
             '("^\\*Occur\\*" (display-buffer-below-selected)))

;;; [ multi-occur ]

;; (use-package multi-occur
;;   :ensure t
;;   :init
;;   (define-key search-prefix (kbd "O") 'multi-occur)
;;   (define-key search-prefix (kbd "M-o") 'multi-occur-in-matching-buffers)
;;   (define-key search-prefix (kbd "M-h") 'how-many)
;;   )


(provide 'init-my-emacs-search-occur)

;;; init-my-emacs-search-occur.el ends here
