;;; init-my-emacs-ebook.el --- init for EBook
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ emacs-ereader ] -- Epub reader for emacs with org-mode integration.

(use-package ereader
  :ensure t
  :config
  (require 'org-ebook)
  ;; (add-to-list 'org-file-apps '("\\.epub\\'" . ereader-mode))
  )


(provide 'init-my-emacs-ebook)

;;; init-my-emacs-ebook.el ends here
