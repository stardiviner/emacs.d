;;; init-my-emacs-search-pt.el --- init for pt
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ platinum searcher (pt) ]

;; (setq pt-arguments "--smart-case")

(unless (boundp 'pt-prefix)
  (define-prefix-command 'pt-prefix))
(define-key my-search-prefix (kbd "p") 'pt-prefix)

(define-key pt-prefix (kbd "p") 'pt-regexp)
(define-key pt-prefix (kbd "r") 'pt-regexp)
(define-key pt-prefix (kbd "f") 'pt-regexp-file-pattern)
(define-key pt-prefix (kbd "P") 'projectile-pt)


;;; [ helm-pt ]

;; (setq helm-pt-args "--smart-case"
;;       helm-pt-insert-at-point t
;;       )

(if (featurep 'helm-pt)
    (progn
      (define-key pt-prefix (kbd "h") 'helm-do-pt)
      (define-key pt-prefix (kbd "o") 'helm-projectile-pt)
      )
  )



(provide 'init-my-emacs-search-pt)

;;; init-my-emacs-search-pt.el ends here
