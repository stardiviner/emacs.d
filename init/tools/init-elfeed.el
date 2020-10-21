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
  :custom ((elfeed-db-directory (expand-file-name ".elfeed" user-emacs-directory))
           (elfeed-curl-extra-arguments '("--proxy" "socks5h://127.0.0.1:1086")) ; specify proxy for elfeed backend cURL.
           ;; (elfeed-log-level 'debug)
           )
  :init (advice-add 'elfeed :after #'elfeed-update) ; auto update after entering elfeed.
  :config
  (setq elfeed-feeds
        '(;; Programming
          ;; ("http://blog.stackoverflow.com/feed/" Programming StackOverflow)
          ;; ("http://programmers.blogoverflow.com/feed/" Programming StackOverflow)
          ;; Emacs
          ("https://planet.emacslife.com/atom.xml" Emacs) ; EmacsLife Planet
          ;; ("https://www.reddit.com/r/emacs/.rss" Emacs Reddit) ; Reddit r/Emacs
          ;; ("https://www.reddit.com/r/orgmode/.rss" Emacs Org-mode Reddit) ; Reddit r/Org-mode
          ("https://sachachua.com/blog/feed/atom/" Blog Emacs) ; Sacha Chua Blog
          ("https://www.bytedude.com/feed.xml" Blog Emacs) ; ByteDude
          ;; Web
          ;; ("https://blog.mozilla.org/feed/" Mozilla) ; Mozilla Blog
          ;; ("http://hacks.mozilla.org/feed/" Mozilla Web) ; Mozilla Hacks
          ;; Linux
          ;; ("https://www.linux.com/rss/feeds.php" Linux)
          ;; ("http://lwn.net/headlines/newrss" Linux)
          ;; ("http://linux.cn/rss.xml" Linux)
          ;; Arch Linux
          ("https://archlinux.org/feeds/planet" Arch Linux) ; Arch Linux RSS
          ;; DevOps
          ;; ("https://www.digitalocean.com/community/tutorials/feed" DigitalOcean Linux)
          ;; Geek News
          ("http://www.solidot.org/index.rss" Solidot News) ; Solidot
          ;; ("http://slashdot.org/index.rss" Slashdot News)
          ;; ("http://news.ycombinator.com/rss" HackerNews Programmer News)
          ;; ("http://reddit.com/.rss" Reddit News)
          ;; ("http://feeds.howtogeek.com/HowToGeek" Geek)
          ("http://fullcirclemagazine.org/feed" Linux) ; Full Circle Magazine
          ;; Podcasts
          ;; Common Lisp
          ;; ("http://planet.lisp.org/rss20.xml" LISP) ; Lisp Planet
          ;; Clojure
          ("http://insideclojure.org/feed.xml" Clojure) ; Inside Clojure
          ("http://planet.clojure.in/atom.xml" Clojure) ; Planet Clojure
          ("http://www.lispcast.com/feed" Clojure) ; LispCast
          ("https://feeds.therepl.net/therepl" Clojure) ; The REPL
          ;; ("https://www.reddit.com/r/Clojure/.rss" Clojure Reddit) ; subreddit "r/Clojure"
          ("https://stackoverflow.com/feeds/tag?tagnames=clojure&sort=newest" Clojure) ; StackOverflow Clojure feed
          ("http://blog.jayfields.com/feeds/posts/default" Clojure Blog)
          ("http://corfield.org/atom.xml" Clojure Blog) ; Sean Corfield Blog
          ("https://www.spacjer.com/feed.xml" Clojure Blog) ; Rafal Spacjer Blog
          ("https://dragan.rocks/feed.xml" Clojure Blog) ; dragan.rocks
          ;; PostgreSQL
          ;; ("https://planet.postgresql.org/rss20.xml" PostgreSQL) ; Planet PostgreSQL
          ;; Programmer Blogs
          ("http://feed.williamlong.info/" Blog News) ; 月光博客
          ("http://www.ruanyifeng.com/blog/atom.xml" Blog) ; 阮一峰
          ("https://manateelazycat.github.io/feed.xml" Blog) ; manateelazycat 懒猫 王勇
          ("https://overreacted.io/rss.xml" Blog)
          ("https://nalaginrut.com/feed/atom" Blog) ; Samson's Machete "穆垒"
          ("https://www.byvoid.com/zht/feed" Blog)
          ("https://shibumi.dev/posts/index.xml" Blog) ; shibumi
          ("https://cireu.github.io/rss.xml" Blog) ; cireu
          ("http://feeds.feedburner.com/felixcat" Blog) ; Felix (felixonmars)
          ;; Kali Linux
          ("http://www.kali.org/feed/" Kali Linux Subscribe) ; Kali Linux Blog
          ;; Subscribe
          ("https://github.com/blog/all.atom" GitHub SubScribe) ; GitHub Blog
          ("http://www.salttiger.com/feed/" ebook) ; Salttiger
          ;; Podcasts
          ("https://feeds.pacific-content.com/commandlineheroes" Programming) ; Command-line Heros podcast
          ))
  
  ;; (define-key elfeed-search-mode-map (kbd "#") 'elfeed-search-set-filter)

  (defalias 'elfeed-search-toggle-all-star ; [m], [*]
    (elfeed-expose #'elfeed-search-toggle-all 'star)
    "Toggle the `star' tag to all selected entries.")
  (define-key elfeed-search-mode-map (kbd "m") 'elfeed-search-toggle-all-star)
  
  (defun elfeed-quit ()
    "Close elfeed buffers."
    (interactive)
    (elfeed-db-save)
    (dolist (buffer '("*elfeed-log*" "*elfeed-search*"))
      (when (buffer-live-p (get-buffer buffer))
        (with-current-buffer buffer
          (kill-buffer)))))
  (define-key elfeed-search-mode-map (kbd "q") 'elfeed-quit)
  (add-hook 'kill-emacs-hook #'elfeed-quit)

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
                (progn
                  (unless (fboundp 'org-web-tools--html-to-org-with-pandoc)
                    (require 'org-web-tools))
                  (let ((org-web-tools-pandoc-sleep-time 5))
                    (org-web-tools--html-to-org-with-pandoc content)))
              (insert content))))))

  (with-eval-after-load 'org-capture
    (add-to-list 'org-capture-templates
                 `("R" ,(format "%s\tcapture elfeed RSS feed content to Org buffer"
                                (all-the-icons-faicon "rss" :face 'all-the-icons-blue-alt))
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

  ;; download with youtube-dl
  (defun youtube-dl-cmd-wrapper (url)
    "Downloads the URL with youtube-dl in an async shell"
    (let ((default-directory "~/Videos"))
      (async-shell-command (format "youtube-dl %s" url))))

  (defun elfeed-youtube-dl (&optional use-generic-p)
    "youtube-dl link"
    (interactive "P")
    (let ((entries (elfeed-search-selected)))
      (cl-loop for entry in entries
               do (elfeed-untag entry 'unread)
               when (elfeed-entry-link entry)
               do (youtube-dl-cmd-wrapper it))
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line))))

  (define-key elfeed-search-mode-map (kbd "d") 'elfeed-youtube-dl)
  )


(provide 'init-elfeed)

;;; init-elfeed.el ends here
