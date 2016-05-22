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
  )



;; (require 'init-my-tool-blog-jekyll)
;; (require 'init-my-tool-blog-octopress)
(require 'init-my-tool-blog-org)


(provide 'init-my-tool-blog)

;;; init-my-tool-blog.el ends here
