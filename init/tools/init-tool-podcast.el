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

;;; [ eradio ] -- A simple Internet radio player for Emacs.

(use-package eradio
  :ensure t
  :commands (eradio-play eradio-stop)
  :custom (eradio-channels '(("def con - soma fm" . "https://somafm.com/defcon256.pls") ; electronica with defcon-speaker bumpers
                             ("metal - soma fm"   . "https://somafm.com/metal130.pls") ; \m/
                             ("cyberia - lainon"  . "https://lainon.life/radio/cyberia.ogg.m3u") ; cyberpunk-esque electronica
                             ("cafe - lainon"     . "https://lainon.life/radio/cafe.ogg.m3u") ; boring ambient, but with lain
                             ))
  )


(provide 'init-tool-podcast)

;;; init-tool-podcast.el ends here
