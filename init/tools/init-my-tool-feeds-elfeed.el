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
        '(("http://blog.stackoverflow.com/feed/" Stack-Overflow)
          ("http://programmers.blogoverflow.com/feed/" Stack-Overflow)

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
          ("http://feeds.feedburner.com/html5rocks" Web)
          ("http://feeds2.feedburner.com/css3" Web)
          ;; ("http://feeds2.feedburner.com/WebDesignerWall" Web)

          ;; Linux
          ("http://openszone.com/feed" Linux)
          ("https://www.linux.com/rss/feeds.php" Linux)

          ;; Arch Linux
          ("http://www.archlinux.org/feeds/news/" Arch)
          ("http://archlinux.me/feed/" Arch)
          ("http://planet.archlinux.org/atom.xml" Arch)
          
          ;; Geek News
          ("http://news.ycombinator.com/rss" geek news)
          ("http://slashdot.org/index.rss" geek news)
          ("http://reddit.com/.rss" geek news)
          ("http://www.solidot.org/index.rss" geek news)
          ("https://projecteuler.net/rss2_euler.xml" geek news)
          ("http://lwn.net/headlines/newrss" geek news)
          ("http://linuxtoy.org/feed/" geek news)
          ("http://linux.cn/rss.xml" geek news)
          ("http://blog.jobbole.com/feed/" geek news)
          ))

  (define-key elfeed-search-mode-map (kbd "#") 'elfeed-search-set-filter)
  ;; "@1-week-ago +unread", "@6-months-ago +unread"
  (setq-default elfeed-search-filter "@1-week-ago +unread")
  ;; (setq elfeed-initial-tags '(unread))

  (define-key my-tools-prefix (kbd "f") 'elfeed)
  )


(provide 'init-my-tool-feeds-elfeed)

;;; init-my-tool-feeds-elfeed.el ends here
