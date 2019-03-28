;;; init-elfeed.el --- init for elfeed
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ elfeed ] -- An Emacs web feeds client

(use-package elfeed
  :ensure t
  :defer t
  :commands (elfeed elfeed-update)
  :bind (:map tools-prefix ("R" . elfeed)
              :map elfeed-search-mode-map ("g" . elfeed-update))
  :init (setq elfeed-db-directory "~/.emacs.d/.elfeed")
  (setq elfeed-search-date-format '("%Y-%m-%d" 10 :right))
  (setq elfeed-feeds
        '(;; Programming
          ("http://blog.stackoverflow.com/feed/" Programming StackOverflow)
          ("http://programmers.blogoverflow.com/feed/" Programming StackOverflow)
          ;; Emacs
          ("http://planet.emacsen.org/atom.xml" Emacs)
          ;; Web
          ("https://blog.mozilla.org/feed/" Mozilla)
          ("http://hacks.mozilla.org/feed/" Mozilla)
          ;; Linux
          ("https://www.linux.com/rss/feeds.php" Linux)
          ("http://lwn.net/headlines/newrss" Linux)
          ("http://linux.cn/rss.xml" Linux)
          ;; Arch Linux
          ("http://www.archlinux.org/feeds/news/" Arch Linux)
          ("http://archlinux.me/feed/" Arch Linux)
          ;; ("http://planet.archlinux.org/atom.xml" Arch)
          ;; DevOps
          ("https://www.digitalocean.com/community/tutorials/feed" DigitalOcean Linux)
          ;; Geek News
          ("http://www.solidot.org/index.rss" Solidot News)
          ;; ("http://slashdot.org/index.rss" Geek)
          ("http://news.ycombinator.com/rss" HackerNews Programmer)
          ;; ("http://reddit.com/.rss" Reddit)
          ("http://blog.jobbole.com/feed/" Jobbole Subscribe)
          ("http://feeds.howtogeek.com/HowToGeek" Linux Geek)
          ("http://fullcirclemagazine.org/feed" Linux Geek)
          ;; Podcasts
          ;; Common Lisp
          ("http://planet.lisp.org/rss20.xml" Lisp)
          ;; Clojure
          ("http://insideclojure.org/feed.xml" Clojure)
          ("http://www.lispcast.com/feed" Clojure)
          ;; Blogs
          ("http://feed.williamlong.info/" Blog)
          ("http://www.ruanyifeng.com/blog/atom.xml" Blog)
          ("http://www.kingname.info/atom.xml" Blog)
          ;; Kali Linux
          ("http://www.kali.org/feed/" Kali Linux Subscribe)
          ;; Subscribe
          ("https://github.com/blog/all.atom" GitHub SubScribe)
          ("http://www.salttiger.com/feed/" Ebook)
          ))

  :config
  ;; (define-key elfeed-search-mode-map (kbd "#") 'elfeed-search-set-filter)
  ;; (setq elfeed-initial-tags '(unread))
  ;; "@1-week-ago +unread", "@6-months-ago +unread"
  ;; (setq-default elfeed-search-filter "@1-week-ago +unread")

  ;; filtering
  (defun elfeed-show-all ()
    (interactive)
    (let ((elfeed-search-filter " "))
      (elfeed-search-live-filter)))
  (define-key elfeed-search-mode-map (kbd "A") 'elfeed-show-all)

  (advice-add 'elfeed-search-untag-all-unread :after ; [r]
              (lambda () (forward-line) (elfeed-search-tag-all 'read)))
  (advice-add 'elfeed-search-show-entry :before ; [Enter]
              (lambda (entry) (elfeed-search-tag-all 'read)))
  (advice-add 'elfeed-search-tag-all-unread :before ; [u]
              (lambda () (elfeed-search-untag-all 'read)))
  (defalias 'elfeed-search-toggle-all-star ; [m], [*]
    (elfeed-expose #'elfeed-search-toggle-all 'star)
    "Toggle the `star' tag to all selected entries.")
  (define-key elfeed-search-mode-map (kbd "*") 'elfeed-search-toggle-all-star)
  (define-key elfeed-search-mode-map (kbd "m") 'elfeed-search-toggle-all-star)

  ;; auto update after entering elfeed.
  (advice-add 'elfeed :after 'elfeed-update)
  
  (defun elfeed-quit ()
    (interactive)
    (elfeed-db-save)
    (quit-window))
  (define-key elfeed-search-mode-map (kbd "q") 'elfeed-quit)
  
  ;; different face colors for different kinds of content (videos, podcast, comics)
  ;; Mapping of tags to faces in the Elfeed entry listing.
  (defface elfeed-unread-tag
    '((t :foreground "light grey"))
    "Mark elfeed tag unread."
    :group 'elfeed)
  (defface elfeed-read-tag
    '((t :foreground "#444444"
         :background "#222222"))
    "Mark elfeed tag read"
    :group 'elfeed)
  (defface elfeed-star-tag
    '((t :foreground "deep pink"
         :background "#222222"))
    "Mark elfeed tag star"
    :group 'elfeed)
  (defface elfeed-podcast-tag
    '((t :foreground "dark magenta"))
    "Mark elfeed podcast tag"
    :group 'elfeed)
  (defface elfeed-programming-tag
    '((t :foreground "yellow green"))
    "Mark elfeed Programming tag"
    :group 'elfeed)
  (defface elfeed-linux-tag
    '((t :foreground "tomato"))
    "Mark elfeed Linux tag"
    :group 'elfeed)
  (defface elfeed-emacs-tag
    '((t :foreground "SteelBlue"))
    "Mark elfeed Emacs tag"
    :group 'elfeed)
  (defface elfeed-arch-tag
    '((t :foreground "light cyan"))
    "Mark elfeed Arch tag"
    :group 'elfeed)
  
  (setq elfeed-search-face-alist
        '((unread elfeed-unread-tag)
          (read elfeed-read-tag)
          (star elfeed-star-tag)
          (Podcast elfeed-podcast-tag)
          (Emacs elfeed-emacs-tag)
          (Linux elfeed-linux-tag)
          (Arch elfeed-arch-tag)
          (Programming elfeed-programming-tag)))
  )

;;; [ elfeed-org ] -- Configure the Elfeed RSS reader with an Org Mode file.

(use-package elfeed-org
  :ensure t
  :defer t
  :after elfeed
  :commands (elfeed-org)
  :init (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed/elfeed.org"))
  :config (elfeed-org))


(provide 'init-elfeed)

;;; init-elfeed.el ends here
