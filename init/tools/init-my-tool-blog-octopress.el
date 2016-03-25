;;; init-my-tool-blog-octopress.el --- init for Octopress
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ octopress ]

;;; Usage:
;;
;; - `octopress-status'
;;      open a new window to show octopress status like Magit. you can operate
;;      in this window.

(use-package octopress
  :ensure t
  :config
  (setq octopress-blog-root "~/Org/Blog/octopress"
        octopress-posts-directory "_posts"
        octopress-drafts-directory "_drafts"
        ;; octopress-default-build-flags ; flags to pass to `jekyll build'
        ;; octopress-default-server-flags ; flags to pass to `jekyll serve'
        )

  (define-key blog-map (kbd "l") 'octopress-status)
  (define-key blog-map (kbd "s") 'octopress-status)
  (define-key blog-map (kbd "n") 'octopress-create-thing)

  (set-face-attribute 'octopress-option-on nil
                      :foreground "cyan")
  (set-face-attribute 'octopress-option-off nil
                      :foreground "dim gray")
  (set-face-attribute 'octopress-highlight-line-face nil
                      :background "dark red" :foreground "white")
  )


;;; [ org-octopress ] --

;;; Org-octopress is a package to help users those who want to write blog
;;; articles in org-style using Octopress (or Jekyll).

;;; How Org-octopress works?
;;
;; Assumed directory tree:
;;
;; + octopress
;;   + source
;;     + blog   <- (1) You compose YYYY-MM-DD-title.org
;;     + _posts <- (2) ox-jekyll.el exports to YYYY-MM-DD-title.html (w/ YAML)
;;   + public
;;     + blog   <- (3) Jekyll exports to YYYY-MM-DD-title.html (w/o YAML).
;;
;; If you have some pictures to be included in your articles, I recommend you to
;; locate them at a sub-directory of blog/. It’ll nice to maintain the relative
;; position with article files.
;;
;; In this work flow, YYYY-MM-DD-title.org will be also published to your web
;; site. If you want to hide the original Org files, you will have to setup
;; Octopress publish settings to exclude org files in your blog/ directory.

;;; Usage:
;;
;; 1. [M-x org-octopress]
;; 2. Compose YYYY-MM-DD-title.org in source/blog directory.
;; 3. C-c C-e (org-export-dispatch) and type “P” “x” “octopress”.
;; 4. In command line terminal: $ rake preview.
;; 5. Check your article in browser.

;; ~/Org/Blog/octopress/

(use-package org-octopress
  ;; :ensure t
  :config
  (setq org-octopress-directory-top       "~/Org/Blog/octopress/source"
        org-octopress-directory-posts     "~/Org/Blog/octopress/source/_posts"
        org-octopress-directory-org-top   "~/Org/Blog/octopress/source"
        org-octopress-directory-org-posts "~/Org/Blog/octopress/source/blog"
        org-octopress-setup-file          "~/sys/lib/org-sty/octopress.org"
        )

  ;; Octopress Settings:
  ;;
  ;; In octopress/_config.yml, you must set the permelink attribute:
  ;;
  ;;     permalink: /blog/:year-:month-:day-:title.html
  )



(provide 'init-my-tool-blog-octopress)

;;; init-my-tool-blog-octopress.el ends here
