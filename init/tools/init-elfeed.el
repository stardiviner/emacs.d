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
  :init (setq elfeed-db-directory (expand-file-name ".elfeed" user-emacs-directory))
  ;; (setq elfeed-log-level 'debug)
  (setq elfeed-search-date-format '("%Y-%m-%d" 10 :right))
  (setq elfeed-feeds
        '(;; Programming
          ("http://blog.stackoverflow.com/feed/" Programming StackOverflow)
          ("http://programmers.blogoverflow.com/feed/" Programming StackOverflow)
          ;; Emacs
          ("https://planet.emacslife.com/atom.xml" Emacs)
          ("https://sachachua.com/blog/feed/atom/" Blog Emacs)
          ;; Web
          ("https://blog.mozilla.org/feed/" Mozilla)
          ("http://hacks.mozilla.org/feed/" Mozilla Web)
          ;; Linux
          ;; ("https://www.linux.com/rss/feeds.php" Linux)
          ;; ("http://lwn.net/headlines/newrss" Linux)
          ;; ("http://linux.cn/rss.xml" Linux)
          ;; Arch Linux
          ("http://www.archlinux.org/feeds/news/" Arch Linux)
          ;; DevOps
          ;; ("https://www.digitalocean.com/community/tutorials/feed" DigitalOcean Linux)
          ;; Geek News
          ("http://www.solidot.org/index.rss" Solidot News)
          ;; ("http://slashdot.org/index.rss" Slashdot News)
          ;; ("http://news.ycombinator.com/rss" HackerNews Programmer News)
          ;; ("http://reddit.com/.rss" Reddit News)
          ;; ("http://feeds.howtogeek.com/HowToGeek" Geek)
          ("http://fullcirclemagazine.org/feed" Linux)
          ;; Podcasts
          ;; Common Lisp
          ("http://planet.lisp.org/rss20.xml" LISP)
          ;; Clojure
          ("http://insideclojure.org/feed.xml" Clojure)
          ("http://www.lispcast.com/feed" Clojure)
          ("http://blog.jayfields.com/feeds/posts/default" Clojure Blog)
          ("http://corfield.org/atom.xml" Clojure Blog) ; Sean Corfield
          ;; Programmer Blogs
          ("http://feed.williamlong.info/" Blog News)
          ("http://www.ruanyifeng.com/blog/atom.xml" Blog)
          ("https://manateelazycat.github.io/feed.xml" Blog)
          ("https://overreacted.io/rss.xml" Blog)
          ("https://nalaginrut.com/feed/atom" Blog) ; Samson's Machete "穆垒"
          ("https://www.byvoid.com/zht/feed" Blog)
          ;; Kali Linux
          ("http://www.kali.org/feed/" Kali Linux Subscribe)
          ;; Subscribe
          ("https://github.com/blog/all.atom" GitHub SubScribe)
          ("http://www.salttiger.com/feed/" Ebook)
          ;; Podcasts
          ("https://feeds.pacific-content.com/commandlineheroes" Programming)
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

  ;; automatically update elfeed
  (defun elfeed-auto-update ()
    (if (get-buffer "*elfeed-search*")
        (with-current-buffer "*elfeed-search*"
          (elfeed-update))))
  (run-with-timer 60 (* 60 60 3) 'elfeed-auto-update)

  ;; support Org Mode Capture template
  (defun my/org-capture-elfeed-title ()
    (with-current-buffer "*elfeed-entry*"
      (elfeed-entry-title elfeed-show-entry)))
  (defun my/org-capture-elfeed-date ()
    (with-current-buffer "*elfeed-entry*"
      (format-time-string
       "[%Y-%m-%d %a %H:%M]"
       (seconds-to-time (elfeed-entry-date elfeed-show-entry)))))
  (defun my/org-capture-elfeed-source ()
    (with-current-buffer "*elfeed-entry*"
      (let ((feed (elfeed-entry-feed elfeed-show-entry)))
        (elfeed-feed-title feed))))
  (defun my/org-capture-elfeed-content ()
    (with-current-buffer "*elfeed-entry*"
      (let* ((content (elfeed-deref (elfeed-entry-content elfeed-show-entry)))
             (type (elfeed-entry-content-type elfeed-show-entry))
             (feed (elfeed-entry-feed elfeed-show-entry))
             (base-url (and feed (elfeed-compute-base (elfeed-feed-url feed)))))
        (if content
            (if (eq type 'html)
                (org-web-tools--html-to-org-with-pandoc content)
              (insert content))))))

  (add-to-list 'org-capture-templates
               '("R" "Capture elfeed [R]SS feed content to Org buffer"
                 entry (file "")
                 "* %(my/org-capture-elfeed-title)
:PROPERTIES:
:SOURCE: %(my/org-capture-elfeed-source)
:DATE(original): %(my/org-capture-elfeed-date)
:DATE: %u
:END:

%(my/org-capture-elfeed-content)"
                 :empty-lines 1
                 :jump-to-captured t)))


;;; [ elfeed-org ] -- Configure the Elfeed RSS reader with an Org Mode file.

;; (use-package elfeed-org
;;   :ensure t
;;   :defer t
;;   :after elfeed
;;   :commands (elfeed-org)
;;   :init (setq rmh-elfeed-org-files (list (expand-file-name "elfeed/elfeed.org" user-emacs-directory)))
;;   :config (elfeed-org))


(provide 'init-elfeed)

;;; init-elfeed.el ends here
