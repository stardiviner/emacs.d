;;; init-my-emacs-ebook.el --- init for EBook
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ emacs-ereader ] -- Epub reader for emacs with org-mode integration.

(use-package ereader
  :ensure t
  :config
  (require 'org-ebook)
  )


(provide 'init-my-emacs-ebook)

;;; init-my-emacs-ebook.el ends here
