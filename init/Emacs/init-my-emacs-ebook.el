;;; init-my-emacs-ebook.el --- init for EBook
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ ereader ] -- Epub reader for emacs with org-mode integration with `ebook:'.

(use-package ereader
  :ensure t
  :load (org-ebook)
  :mode (("\\.epub\\'" . ereader-mode))
  :config
  (with-eval-after-load "org"
    (add-to-list 'org-file-apps '("\\.epub\\'" . auto-mode))))

;;; [ nov ] -- featureful EPUB reader mode.

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (with-eval-after-load "org"
    (add-to-list 'org-file-apps '("\\.epub\\'" . auto-mode))))


(provide 'init-my-emacs-ebook)

;;; init-my-emacs-ebook.el ends here
