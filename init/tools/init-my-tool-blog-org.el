;;; init-my-tool-blog-org.el --- init for Org-mode static site generator.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ org-page ]

(use-package org-page
  :ensure t
  :config
  (setq op/repository-directory (expand-file-name "~/Org/Blog/org-page")
        ;; op/repository-html-branch "master"
        ;; op/repository-org-branch "source"
        op/site-domain "http://stardiviner.github.io/"
        op/site-main-title "stardiviner's blog"
        op/site-sub-title "get over the world!"
        ;; op/theme-root-directory
        ;; op/theme 'mdo
        op/personal-github-link "https://github.com/stardiviner"
        op/personal-avatar "https://stardiviner.github.com/assets/images/avatar.png"
        ;; [ commenting ]
        op/personal-disqus-shortname "stardiviner-dark"
        ;; op/personal-duoshuo-shortname ""
        ;; [ google analytics ]
        op/personal-google-analytics-id "UA-63992234-1"
        )
  )





(provide 'init-my-tool-blog-org)

;;; init-my-tool-blog-org.el ends here
