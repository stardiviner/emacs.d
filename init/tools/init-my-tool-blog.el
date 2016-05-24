;;; init-my-tool-blog.el --- init for Blog
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(unless (boundp 'blog-map)
  (define-prefix-command 'blog-map))
(define-key my-tools-prefix (kbd "l") 'blog-map)


;;; [ blog-admin ] -- Write blog in emacs with many backends: hexo/org-page/nikola

(use-package org-page
  :ensure t
  :config
  (setq op/repository-directory "~/Org/Blog/org-page"
        op/repository-html-branch "gh-pages" ; "master"
        op/repository-org-branch "source"
        op/site-domain "http://stardiviner.github.io/"
        op/site-main-title "stardiviner's blog"
        op/site-sub-title "天地之道"
        op/theme-root-directory (concat user-emacs-directory "org-page/themes")
        op/theme 'peach_blossom
        op/personal-github-link "https://github.com/stardiviner"
        op/personal-avatar "https://stardiviner.github.com/media/img/avatar.png"
        ;; [ commenting ]
        op/personal-disqus-shortname "stardiviner-dark"
        ;; op/personal-duoshuo-shortname ""
        ;; [ google analytics ]
        op/personal-google-analytics-id "63992234"
        )

  (define-key blog-map (kbd "n") 'op/new-post)
  (define-key blog-map (kbd "p") 'op/do-publication)
  (define-key blog-map (kbd "P") 'op/do-publication-and-preview-site)
  )

(use-package blog-admin
  :ensure t
  :config
  ;; open post after create new post
  (add-hook 'blog-admin-backend-after-new-post-hook 'find-file)
  ;; backend: org-page
  (setq blog-admin-backend-type 'org-page
        blog-admin-backend-path "~/Org/Blog/org-page"
        blog-admin-backend-new-post-in-drafts t
        blog-admin-backend-new-post-with-same-name-dir t
        blog-admin-backend-org-page-drafts "_drafts" ; directory to save draft
        )
  )



;; (require 'init-my-tool-blog-jekyll)
;; (require 'init-my-tool-blog-octopress)
;; (require 'init-my-tool-blog-org)



(define-key 'blog-map (kbd "l") 'blog-admin-start)


(provide 'init-my-tool-blog)

;;; init-my-tool-blog.el ends here
