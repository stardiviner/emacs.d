;;; init-my-tool-blog.el --- init for Blog
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(unless (boundp 'blog-map)
  (define-prefix-command 'blog-map))
(define-key my-tools-prefix-map (kbd "b") 'blog-map)


;;; [ jekyll.el ]

;;; Usage:
;;
;; Here are the default key bindings:
;; C-c j d - Show all drafts
;; C-c j p - Show all posts
;; C-c j n - Create new draft
;; C-c j P - Publish current draft
;;
;; To load them just do:
;; (jekyll-init-keybindings)

;; If mumamo is present (it's part of nxhtml) then a multi major mode is enabled
;; for the blog posts, so the YAML front matter will use yaml-mode and the
;; chunks between highlight tags also use their modes.
;;
;; Integrated support for ruby and javascript.
;;
;; You can add other modes for highlighting in your configuration.

(require 'jekyll)

(setq jekyll-directory "~/Servers/Websites/Blogs/org-publish-jekyll"
      ;; jekyll-post-ext ".markdown"
      )

;; (add-to-list 'jekyll-modes-list '("erlang" 'erlang-mode))

(if (featurep 'jekyll)
    (progn
      (unless (boundp 'jekyll-map)
        (define-prefix-command 'jekyll-map))
      (define-key blog-map (kbd "j") 'jekyll-map)
      
      (define-key jekyll-map (kbd "d") 'jekyll-draft-post)
      (define-key jekyll-map (kbd "p") 'jekyll-publish-post)
      (define-key jekyll-map (kbd "P") (defun jekyll-posts-files ()
                                         (interactive)
                                         (find-file (concat jekyll-directory "/_posts/"))))
      (define-key jekyll-map (kbd "D") (defun jekyll-drafts-files ()
                                         (interactive)
                                         (find-file (concat jekyll-directory "/_drafts/"))))
      )
  )


;;; [ org-jekyll ] -- export jekyll blog posts from org-mode.

;; Extracts subtrees from your org-publish project files that have a :blog:
;; keyword and an :on: property with a timestamp, and exports them to a
;; subdirectory _posts of your project's publishing directory in the
;; year-month-day-title.html format that Jekyll expects. Properties are passed
;; over as yaml front-matter in the exported files. The title of the entry is
;; the title of the subtree.

(require 'org-jekyll)

(setq org-jekyll-category nil
      org-jekyll-new-buffers nil
      org-jekyll-localize-dir nil
      org-jekyll-lang-subdirs nil)



(provide 'init-my-tool-blog)

;;; init-my-tool-blog.el ends here
