;;; init-my-org-publish.el --- init for Org Publish.

;;; Commentary:

;;; Code:

(require 'ox-publish)
(require 'ox-rss)

;; export files update style
(setq org-publish-use-timestamps-flag t)

;; [ org-info.js ]
;; (setq org-html-use-infojs t)

;; src code block syntax highlighting
(use-package htmlize
  :ensure t
  :config
  (setq org-html-htmlize-output-type 'css
        org-html-htmlize-font-prefix "org-")
  )

;; MathJax.js
(add-to-list
 'org-html-mathjax-options
 '(path
   "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML"))


(defvar stardiviner-website-html-head
  "<link href='http://fonts.googleapis.com/css?family=Libre+Baskerville:400,400italic' rel='stylesheet' type='text/css'>
<link rel='stylesheet' href='css/site.css' type='text/css'/>")

(defvar stardiviner-website-html-blog-head
  "<link href='http://fonts.googleapis.com/css?family=Libre+Baskerville:400,400italic' rel='stylesheet' type='text/css'>
<link rel='stylesheet' href='../css/site.css' type='text/css'/>")

(defvar stardiviner-website-html-preamble
  "<div class='nav'>
<ul>
<li><a href='/'>Home</a></li>
<li><a href='/Blog/index.html'>Blog</a></li>
<li><a href='/contact.html'>Contact</a></li>
</ul>
</div>")

(defvar stardiviner-website-html-postamble
  "<div class='footer'>
Copyright 2017 %a (%v HTML).<br>
Last updated %C. <br>
Built with %c.
</div>")

(defvar my-org-publish-directory "~/Org-Public")

;; projects definition
(setq org-publish-project-alist
      `(
        ("Website"
         :base-directory (concat org-directory "/Website/")
         :base-extension "org"
         ;; publish to remote with Tramp.
         ;; :publishing-directory "/ssh:user@host#port:/path/to/dir"
         ;; :remote (git "https://github.com/stardiviner/stardiviner.github.com.git" "master")
         :recursive t
         :publishing-directory (concat my-org-publish-directory "/Website/")
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :with-toc nil
         :html-head ,stardiviner-website-html-head
         :html-preamble ,stardiviner-website-html-preamble
         :html-postamble ,stardiviner-website-html-postamble
         :completion-function (my-ox-publish-complete-notify)
         
         ;; [ author info ]
         :with-author "stardiviner"
         :with-email "numbchild@[gmail]"
         :with-footnotes "get over the world!"
         ;; [ latex ]
         :with-sub-superscript t
         :with-tables t
         :with-tags t
         ;; [ tasks ]
         :with-tasks t
         :with-todo-keywords t
         :with-planning t
         :with-timestamps t
         ;; [ html ]
         :html-doctype "html5"
         :html-head-include-default-style t
         :html-head-include-scripts t
         ;; :html-head
         ;; :html-head-extra "<link rel=\"stylesheet\" href=\"assets/stylesheets/stylesheet.css\" type=\"text/css\"/>"
         :html-preamble t
         :html-postamble t
         :html-link-home t
         :html-link-up t
         :html-html5-fancy t
         :html-inline-images t
         :section-numbers t
         :with-toc t
         ;; src code block syntax highlighting
         :htmlized-source t
         ;; [ stylesheet ]
         :style "<link rel=\"stylesheet\" href=\"assets/stylesheets/stylesheet.css\" type=\"text/css\"/>"
         ;; [ sitemap & index ]
         :auto-sitemap t
         :sitemap-title "stardiviner's site"
         :makeindex t
         )

        ("Blog"
         :base-directory (concat org-directory "/Website/Blog/org-publish")
         :base-extension "org"
         :recursive t
         :publishing-directory (concat my-org-publish-directory "/Website/Blog/")
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :with-toc nil
         :html-head ,stardiviner-website-html-blog-head
         :html-head-extra
         "<link rel=\"alternate\" type=\"application/rss+xml\"
                href=\"https://stardiviner.github.io/Blog/Blog.xml\"
                title=\"RSS feed\">"
         :html-preamble ,stardiviner-website-html-preamble
         :html-postamble ,stardiviner-website-html-postamble

         :completion-function (my-ox-publish-sync)
         )

        ("images"
         :base-directory (concat org-directory "/Website/images/")
         :base-extension "jpg\\|gif\\|png"
         :publishing-directory (concat my-org-publish-directory "/Website/images/")
         :publishing-function org-publish-attachment)

        ("js"
         :base-directory (concat org-directory "/Website/js/")
         :base-extension "js"
         :publishing-directory (concat my-org-publish-directory "/Website/js/")
         :publishing-function org-publish-attachment)

        ("css"
         :base-directory (concat org-directory "/Website/css/")
         :base-extension "css"
         :publishing-directory (concat my-org-publish-directory "/Website/css/")
         :publishing-function org-publish-attachment)

        ("rss"
         :base-directory (concat org-directory "/Website/Blog/org-publish/")
         :base-extension "org"
         :publishing-directory (concat my-org-publish-directory "/Website/Blog")
         :publishing-function (org-rss-publish-to-rss)
         :html-link-home "https://stardiviner.github.io/"
         :html-link-use-abs-url t)

        ("static"
         :base-directory (concat org-directory "/Website/Blog/org-publish/")
         :base-extension "pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/Org/Blog/org-publish/exported_html/Blog"
         :recursive t
         :publishing-function org-publish-attachment
         )
        
        ("website" :components (
                                ;; "Org"
                                "Blog"
                                "images" "js" "css"
                                "rss"))
        ))


(defun my-ox-publish-complete-notify ()
  (notifications-notify :title "org-publish" :body "completed."))

(defun my-ox-publish-sync ()
  "Sync ox-publish exported files to remote server."

  ;; Blog source
  (magit-status
   (concat org-directory "/Website/Blog"))
  (magit-stage-modified)
  (magit-stage-untracked)
  (magit-commit-add-log)
  (magit-push-current-to-upstream)

  ;; Blog exported files
  (magit-status
   (concat my-org-publish-directory "/Website/Blog"))
  (magit-stage-modified)
  (magit-stage-untracked)
  (magit-commit-add-log)
  (magit-push-current-to-upstream)
  )

(add-to-list
 'org-capture-templates
 '("b" "[B]log"
   entry (file+olp+datetree (concat org-directory "/Website/Blog/org-publish/index.org"))
   "\n* %^{blog title}\n:PROPERTIES:\n:TIME: %U\n:END: \n\n[[file:%<%Y-%m-%d %R>.org][%^{blog title}]]\n\n%i"
   :empty-lines 1
   :jump-to-captured t
   ))




(provide 'init-my-org-publish)

;;; init-my-org-publish.el ends here
