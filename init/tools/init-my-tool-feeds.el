;;; init-my-tool-feeds.el --- init for feeds in Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ elfeed ] -- An Emacs web feeds client

;;; Usage:
;;
;; - `elfeed' :: run elfeed.
;; - import feeds:
;;   - `elfeed-load-opml'
;; - export feeds:
;;   - `elfeed-export-opml'

(define-key my-tools-prefix-map (kbd "f") 'elfeed)


;; (require 'init-my-tool-feeds-newsticker)



(provide 'init-my-tool-feeds)

;;; init-my-tool-feeds.el ends here
