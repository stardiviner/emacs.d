;;; init-my-tool-feeds-newsticker.el --- init for newssticker
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(require 'newsticker)

;; W3M HTML renderer isn't essential, but it's pretty useful.
(autoload 'w3m "w3m")
(eval-after-load 'w3m
  (setq newsticker-html-renderer 'w3m-region))

;; We want our feeds pulled every 30 minutes.
(setq newsticker-retrieval-method 'extern ; 'intern
      newsticker-retrieval-interval 1800)

;; Optionally bind a shortcut for your new RSS reader.
(global-set-key (kbd "C-c r") 'newsticker-treeview)

;; Setup the feeds. We'll have a look at these in just a second.
(setq newsticker-url-list-defaults
      '(("Emacs Wiki" "http://www.emacswiki.org/cgi-bin/wiki.pl?action=rss" nil 3600)))
(setq newsticker-url-list
      '( ;; Arch Linux
        ("Arch Linux news" "http://www.archlinux.org/feeds/news/")
        ("Planet Arch Linux" "http://planet.archlinux.org/atom.xml")
        ("Arch Linux User Blogs" "http://archlinux.me/?type=atom")
        ;; Emacs
        ("Planet Emacs" "http://planet.emacsen.org/atom.xml")
        ))

(setq newsticker-dir "~/.emacs.d/newsticker/"
      newsticker-groups '(("Emacs" "Emacs Wiki")
                          ("Arch Linux" "Arch Linux news" ("Arch Linux Packages News" "Arch All repos packages") "Planet Arch Linux")
                          )
      newsticker-frontend 'newsticker-treeview
      newsticker-keep-obsolete-items t
      newsticker-new-item-functions 'newsticker-download-images
      newsticker-scroll-smoothly t
      )

;;; face
(set-face-attribute 'newsticker-default-face nil
                    :foreground "gray")
;; old item
(set-face-attribute 'newsticker-old-item-face nil
                    :foreground "black")
;; new item
(set-face-attribute 'newsticker-new-item-face nil
                    :foreground "green yellow")
;; enclosure
(set-face-attribute 'newsticker-enclosure-face nil
                    :foreground "red")
;; statistics
(set-face-attribute 'newsticker-statistics-face nil
                    :foreground "yellow")
;; headline of body
(set-face-attribute 'newsticker-feed-face nil
                    :foreground "cyan"
                    :weight 'bold
                    :underline t)
;; date
(set-face-attribute 'newsticker-date-face nil
                    :foreground "dim gray")
;; body end author etc info
(set-face-attribute 'newsticker-extra-face nil
                    :foreground "orange")
(set-face-attribute 'newsticker-immortal-item-face nil
                    :foreground "dark red")
(set-face-attribute 'newsticker-obsolete-item-face nil
                    :foreground "#444444")

;;; treeview colors:
;; newsticker treeview normal face
(set-face-attribute 'newsticker-treeview-face nil
                    :foreground "gray")
(set-face-attribute 'newsticker-treeview-new-face nil
                    :foreground "green yellow")
(set-face-attribute 'newsticker-treeview-old-face nil
                    :foreground "dim gray")
(set-face-attribute 'newsticker-treeview-immortal-face nil
                    :foreground "dark red")
(set-face-attribute 'newsticker-treeview-obsolete-face nil
                    :foreground "#444444")
(set-face-attribute 'newsticker-treeview-selection-face nil
                    :foreground "white" :background "#004A5D"
                    :box '(:color "cyan" :line-width -1 :style nil)
                    :weight 'bold)



(provide 'init-my-tool-feeds-newsticker)

;;; init-my-tool-feeds-newsticker.el ends here
