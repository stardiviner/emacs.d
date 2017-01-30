;;; init-my-emacs-search-find.el --- init for command find
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

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


(provide 'init-my-emacs-search-find)

;;; init-my-emacs-search-find.el ends here
