;;; init-my-emacs-search-find.el --- init for command find
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'find-prefix)
  (define-prefix-command 'find-prefix))

(define-key my-search-prefix (kbd "f") 'find-prefix)

;;; [ f3 ] -- The Fantastic File Finder: a helm interface for searching files really fast.

(use-package f3
  :ensure t
  :bind ("C-x f" . f3)
  )

;;; [ fuff ] -- Find files with findutils recursively as a replacement of `find-file'.

(use-package fuff
  :ensure t
  :bind ("C-x C-f" . fuff-find-file)
  )

;;; [ helm-fuzzy-find ] -- Find files using Fuzzy Search (fuzzy-find) with Helm.

(use-package helm-fuzzy-find
  :ensure t
  :bind (("C-c C-/" . helm-fuzzy-find)
         :map find-prefix
         ("f" . helm-fuzzy-find))
  :config
  ;; (setq helm-fuzzy-find-keybind "")
  )


(provide 'init-my-emacs-search-find)

;;; init-my-emacs-search-find.el ends here
