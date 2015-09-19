;;; init-my-tool-bookmark.el --- init for Browser Bookmarks
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ helm-firefox ]

;;; `helm-firefox-bookmarks'

(require 'helm-firefox)

(setq helm-firefox-default-directory "~/.moizlla/firefox")


;;; [ helm-chrome ]




(cond
 ((featurep 'helm-firefox)
  (define-key my-tools-prefix (kbd "b") 'helm-firefox-bookmarks))
 ((featurep 'helm-chrome)
  (define-key my-tools-prefix (kbd "b") 'helm-firefox-bookmarks))
 )


(provide 'init-my-tool-bookmark)

;;; init-my-tool-bookmark.el ends here
