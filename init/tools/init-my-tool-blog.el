;;; init-my-tool-blog.el --- init for Blog
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(unless (boundp 'blog-map)
  (define-prefix-command 'blog-map))
(define-key my-tools-prefix (kbd "l") 'blog-map)


;;; [ blog-admin ] -- Write blog in emacs with many backends: hexo/org-page/nikola

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
(require 'init-my-tool-blog-org)


(provide 'init-my-tool-blog)

;;; init-my-tool-blog.el ends here
