;;; init-my-tool-blog-jekyll.el --- init for Jekyll
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ jekyll.el ]

;;; Usage:
;;
;; commands usage workflow
;;
;; 1. create a new draft
;; 2. publish current draft in draft buffer
;; 3. build
;;
;; Here are the default key bindings:
;; C-x t l j D - Show all drafts
;; C-x t l j P - Show all posts
;; C-x t l j d - Create new draft
;; C-x t l j p - Publish current draft
;; C-x t l j b - Build
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

;; (require 'jekyll)
;;
;; (setq jekyll-directory "~/Servers/Websites/Blogs/jekyll.github.io/"
;;       ;; jekyll-post-ext ".markdown"
;;       )

;; (setq jekyll-post-template "---
;; title: %s
;; ---
;;
;; ")

;; (require 'jekyll-mumamo)
;;; You can add other modes for highlighting in your configuration.
;;; Integrated support for ruby and javascript.
;; (add-to-list 'jekyll-modes-list '("erlang" 'erlang-mode))

;; (if (featurep 'jekyll)
;;     (progn
;;       (unless (boundp 'jekyll-map)
;;         (define-prefix-command 'jekyll-map))
;;       (define-key blog-map (kbd "j") 'jekyll-map)
;;      
;;       (define-key jekyll-map (kbd "d") 'jekyll-draft-post)
;;       (define-key jekyll-map (kbd "p") 'jekyll-publish-post)
;;       (define-key jekyll-map (kbd "P") (defun jekyll-posts-files ()
;;                                          (interactive)
;;                                          (find-file (concat jekyll-directory "_posts/"))))
;;       (define-key jekyll-map (kbd "D") (defun jekyll-drafts-files ()
;;                                          (interactive)
;;                                          (find-file (concat jekyll-directory "_drafts/"))))
;;       (define-key jekyll-map (kbd "b")
;;         (defun jekyll-build ()
;;           (interactive)
;;           (shell-command "jekyll build")))
;;       )
;;   )


;;; [ jekyll-modes ]

;;; Usage:
;;
;; - [jekyll-markdown-mode]
;; - [jekyll-html-mode]


(provide 'init-my-tool-blog-jekyll)

;;; init-my-tool-blog-jekyll.el ends here
