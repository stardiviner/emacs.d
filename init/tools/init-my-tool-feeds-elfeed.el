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

  (advice-add 'elfeed-search-untag-all-unread
              :after (lambda ()
                       (previous-line)
                       (elfeed-search-tag-all 'read)))
  (advice-add 'elfeed-search-show-entry
              :before (lambda (entry)
                        (elfeed-search-tag-all 'read)))
  (advice-add 'elfeed-search-tag-all-unread
              :before (lambda ()
                        (elfeed-search-untag-all 'read)))
  (defalias 'elfeed-search-toggle-all-star
    (elfeed-expose #'elfeed-search-toggle-all 'star)
    "Toggle the `star' tag to all selected entries.")
  (define-key elfeed-search-mode-map (kbd "*") 'elfeed-search-toggle-all-star)
  (define-key elfeed-search-mode-map (kbd "m") 'elfeed-search-toggle-all-star)
  
  ;; different face colors for different kinds of content (videos, podcast, comics)
  ;; Mapping of tags to faces in the Elfeed entry listing.
  (defface elfeed-unread-tag
    '((t :foreground "light grey"))
    "Mark elfeed tag unread.")
  (defface elfeed-read-tag
    '((t :foreground "#444444"
         :background (color-darken-name (face-background 'default) 3)))
    "Mark elfeed tag read")
  (defface elfeed-star-tag
    '((t :foreground "deep pink"
         :background (color-darken-name (face-background 'default) 2)))
    "Mark elfeed tag star")
  (defface elfeed-podcast-tag
    '((t :foreground "magenta"))
    "Mark elfeed podcast tag")
  (defface elfeed-programming-tag
    '((t :foreground "yellow green"))
    "Mark elfeed Programming tag")
  (defface elfeed-linux-tag
    '((t :foreground "tomato"))
    "Mark elfeed Linux tag")
  (defface elfeed-emacs-tag
    '((t :foreground "SteelBlue"))
    "Mark elfeed Emacs tag")
  (defface elfeed-arch-tag
    '((t :foreground "light cyan"))
    "Mark elfeed Arch tag")
  
  (setq elfeed-search-face-alist
        '((unread elfeed-unread-tag)
          (read elfeed-read-tag)
          (star elfeed-star-tag)
          (Emacs elfeed-emacs-tag)
          (Linux elfeed-linux-tag)
          (Arch elfeed-arch-tag)
          (Programming elfeed-programming-tag)))
  
  (define-key my-tools-prefix (kbd "f") 'elfeed)
  )


(provide 'init-my-tool-feeds-elfeed)

;;; init-my-tool-feeds-elfeed.el ends here
