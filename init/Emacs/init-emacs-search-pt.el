;;; init-emacs-search-pt.el --- init for pt
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ platinum searcher (pt) ]

(use-package pt
  :ensure t
  :config
  ;; (setq pt-arguments "--smart-case")

  (unless (boundp 'pt-prefix)
    (define-prefix-command 'pt-prefix))
  (define-key search-prefix (kbd "p") 'pt-prefix)

  (define-key pt-prefix (kbd "p") 'pt-regexp)
  (define-key pt-prefix (kbd "r") 'pt-regexp)
  (define-key pt-prefix (kbd "f") 'pt-regexp-file-pattern)
  (define-key pt-prefix (kbd "P") 'projectile-pt)
  )


(provide 'init-emacs-search-pt)

;;; init-emacs-search-pt.el ends here
