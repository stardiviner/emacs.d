;;; init-my-tool-blog.el --- init for Blog
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(unless (boundp 'blog-map)
  (define-prefix-command 'blog-map))
(global-set-key (kbd "C-c t b") 'blog-map)


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

;; (add-to-list 'jekyll-modes-list '("erlang" 'erlang-mode))

(if (featurep 'jekyll)
    (progn
      (unless (boundp 'jekyll-map)
        (define-prefix-command 'jekyll-map))
      (define-key blog-map (kbd "j") 'jekyll-map)
      
      (define-key jekyll-map (kbd "d") 'jekyll-draft-post)
      (define-key jekyll-map (kbd "P") 'jekyll-publish-post)
      (define-key jekyll-map (kbd "p") (lambda ()
                                         (interactive)
                                         (find-file (concat jekyll-directory "_posts/"))))
      (define-key jekyll-map (kbd "d") (lambda ()
                                         (interactive)
                                         (find-file (concat jekyll-directory "_drafts/"))))
      )
  )


;;; [ org-jekyll ] --



(provide 'init-my-tool-blog)

;;; init-my-tool-blog.el ends here
