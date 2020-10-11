;;; init-tool-podcast.el --- init for Podcast in Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ podcaster ] -- an Emacs podcast client.

(use-package podcaster
  :ensure t
  :defer t
  :commands (podcaster)
  :bind (:map tools-prefix ("P" . podcaster))
  :custom (podcaster-feeds-urls
           '("https://ipn.li/kernelpanic/feed"
             "http://sachachua.com/blog/tag/emacs-chat/podcast")))


(provide 'init-tool-podcast)

;;; init-tool-podcast.el ends here
