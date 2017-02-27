;;; init-my-tool-blog-org.el --- init for Org-mode static site generator.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ org-page ]

(use-package org-page
  :ensure t
  :defer t
  :bind (:map blog-map
              ("n" . op/new-post)
              ("p" . op/do-publication)
              ("C-p" . op/do-publication-and-preview-site))
  :config
  (setq op/repository-directory (expand-file-name "~/Org/Blog/org-page")
        ;; op/repository-html-branch "master"
        ;; op/repository-org-branch "source"
        op/site-domain "http://stardiviner.github.io/"
        op/site-main-title "stardiviner blog"
        op/site-sub-title "大道无极"
        op/theme-root-directory (concat user-emacs-directory "org-page/themes")
        op/theme 'peach_blossom
        op/personal-github-link "https://github.com/stardiviner"
        op/personal-avatar "https://stardiviner.github.com/media/img/avatar.png"
        ;; [ commenting ]
        op/personal-disqus-shortname "stardiviner-dark"
        ;; op/personal-duoshuo-shortname ""
        ;; [ google analytics ]
        op/personal-google-analytics-id "63992234"
        op/category-ignore-list '("themes" "assets" "blog")
        )

  (setq op/category-config-alist
        '(("blog" ; this is the default configuration
           :label "Blog"
           :show-meta t
           :show-comment t
           :uri-generator op/generate-uri
           :uri-template "/blog/%y/%m/%d/%t/"
           :sort-by :date ; how to sort posts
           :category-index t ; generate category index or not
           )
          ("index"
           :show-meta nil
           :show-comment nil
           :uri-generator op/generate-uri
           :uri-template "/"
           :sort-by :date
           :category-index nil
           )
          ("about"
           :show-meta nil
           :show-comment nil
           :uri-generator op/generate-uri
           :uri-template "/about/"
           :sort-by :date
           :category-index nil)
          ))
  )




(provide 'init-my-tool-blog-org)

;;; init-my-tool-blog-org.el ends here
