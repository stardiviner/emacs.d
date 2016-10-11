;;; init-my-tool-feeds-elfeed.el --- init for elfeed
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ elfeed ] -- An Emacs web feeds client

(use-package elfeed
  :ensure t
  :config
  (setq elfeed-db-directory "~/.emacs.d/.elfeed")

  (setq elfeed-feeds
        '(("http://blog.stackoverflow.com/feed/" Programming)
          ("http://programmers.blogoverflow.com/feed/" Programming)

          ;; Emacs
          ("http://planet.emacsen.org/atom.xml" Emacs)

          ;; Ruby
          ;; ("http://www.ruby-lang.org/en/feeds/news.rss" Ruby)
          ;; ("http://rubyweekly.com/rss" Ruby)
          ;; ("http://feeds.feedburner.com/RubyInside" Ruby)
          ;; ("http://feeds2.feedburner.com/Rubyflow" Ruby)
          ;; ("http://feeds.feedburner.com/Ruby5" Ruby)

          ;; Web
          ("https://blog.mozilla.org/feed/" Web)
          ("http://hacks.mozilla.org/feed/" Web)

          ;; Linux
          ("http://openszone.com/feed" Linux)
          ("https://www.linux.com/rss/feeds.php" Linux)

          ;; Arch Linux
          ("http://www.archlinux.org/feeds/news/" Arch)
          ("http://archlinux.me/feed/" Arch)
          ("http://planet.archlinux.org/atom.xml" Arch)
          
          ;; Geek News
          ("http://news.ycombinator.com/rss" Geek)
          ("http://slashdot.org/index.rss" Geek)
          ("http://reddit.com/.rss" Geek)
          ("http://www.solidot.org/index.rss" Geek)
          ("https://projecteuler.net/rss2_euler.xml" Geek)
          ("http://lwn.net/headlines/newrss" Geek)
          ("http://linuxtoy.org/feed/" Geek)
          ("http://linux.cn/rss.xml" Geek)
          ("http://blog.jobbole.com/feed/" Geek)
          ))

  (define-key elfeed-search-mode-map (kbd "#") 'elfeed-search-set-filter)
  ;; (setq elfeed-initial-tags '(unread))
  ;; "@1-week-ago +unread", "@6-months-ago +unread"
  ;; (setq-default elfeed-search-filter "@1-week-ago +unread")

  (defun elfeed-mark-read ()
    (interactive)
    ;; (elfeed-search-untag-all-unread)
    (elfeed-search-untag-all 'unread)
    (previous-line)
    (elfeed-search-tag-all 'read)
    )
  (define-key elfeed-search-mode-map (kbd "r") 'elfeed-mark-read)
  
  ;; different face colors for different kinds of content (videos, podcast, comics)
  ;; Mapping of tags to faces in the Elfeed entry listing.
  (setq elfeed-search-face-alist
        '((unread (:foreground "light grey"))
          (read (:background (color-darken-name (face-background 'default) 3)
                             :foregroound "#444444"))
          (Emacs (:foreground "SteelBlue"))
          (Linux (:foreground "tomato"))
          (Arch (:foreground "cyan"))
          (Programming
           (:background (color-darken-name (face-background 'default) 3)
                        :foreground "YellowGreen"
                        ))))
  
  (define-key my-tools-prefix (kbd "f") 'elfeed)
  )


(provide 'init-my-tool-feeds-elfeed)

;;; init-my-tool-feeds-elfeed.el ends here
