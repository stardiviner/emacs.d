;;; init-my-emacs-ebook.el --- init for EBook
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ ereader ] -- Epub reader for emacs with org-mode integration with `ebook:'.

(use-package ereader
  :ensure t
  :mode (("\\.epub\\'" . ereader-mode))
  :config
  (with-eval-after-load "org"
    (add-to-list 'org-file-apps '("\\.epub\\'" . auto-mode)))
  (require 'org-ebook)
  (org-link-set-parameters "ebook"
                           :follow #'org-ebook-open
                           :store #'org-ebook-store-link
                           :complete #'org-file-complete-link))

;;; [ nov ] -- featureful EPUB reader mode.

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (with-eval-after-load "org"
    (add-to-list 'org-file-apps '("\\.epub\\'" . auto-mode)))
  ;; (define-key nov-mode-map (kbd "q") 'kill-current-buffer)
  )


(provide 'init-my-emacs-ebook)

;;; init-my-emacs-ebook.el ends here
