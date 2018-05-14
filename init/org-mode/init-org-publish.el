;;; init-org-publish.el --- init for Org Publish.

;;; Commentary:

;;; Code:

(require 'ox-publish)
(require 'ox-html)
(require 'ox-rss)

(defvar my-org-publish-source "~/Org/Website/")
(defvar my-org-publish-destination "~/org-publish/")

;;; [ ox-html ]

;;; Allow displaying source code block using "klipse".
(setq org-html-klipsify-src t)

(setq org-html-allow-name-attribute-in-anchors t)

;;; org-info.js
(setq org-html-infojs-options
      '((path . "https://orgmode.org/org-info.js")
        (view . "showall") ; "info", "overview", "content", "showall"
        (toc . :with-toc)
        (ftoc . "0") ; fixed TOC
        (tdepth . "max")
        (sdepth . "max")
        (mouse . "underline")
        (buttons . "0")
        (ltoc . "1") ; local TOC
        (up . :html-link-up)
        (home . :html-link-home)))

;;; [ ox-org ]
;;; support "Show Org Source" (.org.html version) button.
(require 'ox-org) ; `org-org-publish-to-org'
(require 'htmlize)

;;; - inline CSS (better colors support for src blocks)
;;; use a specific theme for `ox-org'.
(setq org-html-htmlize-output-type 'inline-css)
;;; FIXME: this seems does not work.
(setq my-org-html-export-theme 'spacemacs-dark)
(defun my-ox-org-with-theme (orig-fun &rest args)
  (load-theme my-org-html-export-theme)
  (unwind-protect
      (apply orig-fun args)
    (disable-theme my-org-html-export-theme)))
(with-eval-after-load "ox-org"
  (advice-add 'org-export-to-buffer :around 'my-ox-org-with-theme))

;; projects definition
(setq org-publish-project-alist
      `(("blog-org"
         :base-directory ,(concat my-org-publish-source "Blog/")
         :base-extension "org"
         :recursive t
         :exclude-tags ("noexport" "todo")
         :publishing-function (org-html-publish-to-html org-org-publish-to-org)
         :publishing-directory ,(concat my-org-publish-destination "Blog/")

         ;; TODO: publish to remote with Tramp.
         ;; :publishing-directory "/ssh:stardiviner@github.com:#port:/path/to/dir"
         ;; :publishing-directory "git@github.com:stardiviner/stardiviner.github.io.git"
         ;; :remote (git "https://github.com/stardiviner/stardiviner.github.com.git" "gh-pages")

         ;; [ author info ]
         :with-author "stardiviner"
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
         :html-html5-fancy t ; use new HTML5 elements.
         :html-head-include-scripts t
         :html-link-home "https://stardiviner.github.io/" ; or "/"
         ;; :html-home/up-format ""
         :html-preamble ,(org-file-contents (concat my-org-publish-source "assets/preamble.html"))
         :html-postamble ,(org-file-contents (concat my-org-publish-source "assets/postamble.html"))
         :html-link-org-files-as-html t
         :section-numbers t
         :with-toc nil
         :table-of-contents nil
         ;; [ Show Org source ]
         ;; use external CSS stylesheet instead.
         :htmlized-source t ; toggle for `org-org-publish-to-org'
         ;; NOTE: conflict with customize font-lock: font-lock-add-keywords defined org faces.
         ;; [ src blocks ]
         :html-klipsify-src t
         ;; [ images ]
         :html-inline-images t
         ;; [ stylesheet ]
         :html-head-include-default-style t
         :html-head-extra ,(concat "<link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/stylesheets/stylesheet.css\"/>"
                                   "<link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/stylesheets/syntax-highlight.css\"/>"
                                   ;; RSS
                                   "<link rel=\"alternate\" type=\"application/rss+xml\" href=\"https://stardiviner.github.io/Blog/index.xml\">"
                                   ;; "<script type=\"text/javascript\" language=\"JavaScript\" src=\"org-info.js\"></script>"
                                   ;; icon
                                   "<link ref=\"icon\" type=\"image/png\" href=\"/assets/images/favicon-16x16.png\" sizes=\"16x16\">"
                                   "<link ref=\"icon\" type=\"image/png\" href=\"/assets/images/favicon-32x32.png\" sizes=\"32x32\">"
                                   ;; MathJax
                                   "<script type=\"text/javascript\" src=\"https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-AMS_HTML\"></script>"
                                   )
         ;; :style "This is raw HTML for stylesheet <link>'s"
         ;; [ sitemap ]
         ;; :auto-sitemap t ; generate sitemap.html
         ;; [ index ]
         ;; :makeindex t ; generate theindex.org and publish it as theindex.html
         ;; [ info.js]
         :html-use-infojs t
         ;; [ MathJax.js ]
         :html-mathjax-options ((path "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-AMS_HTML"))
         :html-mathjax-template "<script type=\"text/javascript\" src=\"%PATH\"></script>"
         )

        ("blog-RSS"
         :base-directory ,(concat my-org-publish-source "Blog/")
         :base-extension "org"
         :publishing-directory ,(concat my-org-publish-destination "Blog")
         :publishing-function (org-rss-publish-to-rss) ; generate index.xml
         ;; :rss-image-url "https://stardiviner.github.io/assets/images/RSS.png"
         :rss-extension "xml"
         :html-link-home "https://stardiviner.github.io/"
         :html-link-use-abs-url t
         :exclude ".*" ; exclude all other files...
         :include ("index.org") ; ... except index.org.
         :section-numbers nil
         :table-of-contents nil)

        ("blog-data"
         :base-directory ,(concat my-org-publish-source "Blog/data")
         :base-extension any
         :publishing-directory ,(concat my-org-publish-destination "Blog/data")
         :recursive t
         :publishing-function org-publish-attachment)
        ))

(add-to-list 'org-publish-project-alist
             '("Blog" :components ("blog-org" "blog-RSS" "blog-data")))

(add-to-list 'org-publish-project-alist
             `("assets"                  ; website materials: "JS", "CSS", "Images" etc.
               :base-directory ,(concat my-org-publish-source "assets")
               :base-extension any
               :publishing-directory ,(concat my-org-publish-destination "assets/")
               :recursive t
               :publishing-function org-publish-attachment))

(add-to-list 'org-publish-project-alist
             `("Index"
               :base-directory ,(concat org-directory "/Website")
               :base-extension "org"
               :exclude ".*" ; exclude all other files...
               :include ("index.org") ; ... except index.org.
               :recursive nil
               :publishing-directory ,(expand-file-name my-org-publish-destination)
               :publishing-function org-html-publish-to-html
               
               ;; [ html ]
               :html-doctype "html5"
               :html-html5-fancy t ; use new HTML5 elements.
               :html-head-include-scripts t
               :html-link-home "https://stardiviner.github.io/"
               :html-preamble ,(org-file-contents (concat my-org-publish-source "assets/preamble.html"))
               :html-postamble ,(org-file-contents (concat my-org-publish-source "assets/postamble.html"))
               :html-link-org-files-as-html t
               :section-numbers t
               :with-toc nil
               :table-of-contents nil
               ;; [ src code block ]
               :htmlized-source nil ; src code block syntax highlighting
               ;; [ images ]
               :html-inline-images t
               ;; [ stylesheet ]
               :html-head-include-default-style t
               :html-head-extra ,(concat "<link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/stylesheets/stylesheet.css\"/>"
                                         "<link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/stylesheets/syntax-highlight.css\"/>"
                                         ;; icon
                                         "<link ref=\"icon\" type=\"image/png\" href=\"/assets/images/favicon-16x16.png\" sizes=\"16x16\">"
                                         "<link ref=\"icon\" type=\"image/png\" href=\"/assets/images/favicon-32x32.png\" sizes=\"32x32\">"
                                         )
               ;; [ info.js]
               :html-use-infojs nil

               ;; NOTE: `:completion-function' must be in the last component in `org-publish-project-alist'.
               :completion-function (my-org-publish-sync my-org-publish-finished-notify)
               ))

(add-to-list 'org-publish-project-alist
             `("about-data"
               :base-directory ,(concat my-org-publish-source "About/data")
               :base-extension any
               :publishing-directory ,(concat my-org-publish-destination "About/data")
               :recursive t
               :publishing-function org-publish-attachment))

(add-to-list 'org-publish-project-alist
             `("about-org"
               :base-directory ,(concat my-org-publish-source "About")
               :base-extension "org"
               :recursive t
               :publishing-directory ,(concat my-org-publish-destination "About")
               :publishing-function (org-html-publish-to-html)

               ;; [ html ]
               :html-doctype "html5"
               :html-html5-fancy t ; use new HTML5 elements.
               :html-head-include-scripts t
               :html-link-home "https://stardiviner.github.io/"
               :html-preamble ,(org-file-contents (concat my-org-publish-source "assets/preamble.html"))
               :html-postamble ,(org-file-contents (concat my-org-publish-source "assets/postamble.html"))
               :html-link-org-files-as-html t
               :section-numbers t
               :with-toc nil
               :table-of-contents nil
               ;; [ src code block ]
               :htmlized-source nil ; src code block syntax highlighting
               ;; [ images ]
               :html-inline-images t
               ;; [ stylesheet ]
               :html-head-include-default-style t
               :html-head-extra ,(concat "<link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/stylesheets/stylesheet.css\"/>"
                                         "<link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/stylesheets/syntax-highlight.css\"/>"
                                         ;; icon
                                         "<link ref=\"icon\" type=\"image/png\" href=\"/assets/images/favicon-16x16.png\" sizes=\"16x16\">"
                                         "<link ref=\"icon\" type=\"image/png\" href=\"/assets/images/favicon-32x32.png\" sizes=\"32x32\">"
                                         )
               ;; [ info.js]
               :html-use-infojs t
               ))

(add-to-list 'org-publish-project-alist
             '("About" :components ("about-org" "about-data")))

(add-to-list 'org-publish-project-alist
             `("poem-data"
               :base-directory ,(concat my-org-publish-source "Poem/data")
               :base-extension any
               :publishing-directory ,(concat my-org-publish-destination "Poem/data")
               :recursive t
               :publishing-function org-publish-attachment))

(add-to-list 'org-publish-project-alist
             `("poem-org"
               :base-directory ,(concat my-org-publish-source "Poem")
               :base-extension "org"
               :recursive t
               :publishing-directory ,(concat my-org-publish-destination "Poem")
               :publishing-function org-html-publish-to-html
               
               ;; [ html ]
               :html-doctype "html5"
               :html-html5-fancy t ; use new HTML5 elements.
               :html-head-include-scripts t
               :html-link-home "https://stardiviner.github.io/"
               :html-preamble ,(org-file-contents (concat my-org-publish-source "assets/preamble.html"))
               :html-postamble ,(org-file-contents (concat my-org-publish-source "assets/postamble.html"))
               :html-link-org-files-as-html t
               :section-numbers t
               :with-toc nil
               :table-of-contents nil
               ;; [ src code block ]
               :htmlized-source nil ; src code block syntax highlighting
               ;; [ images ]
               :html-inline-images t
               ;; [ stylesheet ]
               :html-head-include-default-style t
               :html-head-extra ,(concat "<link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/stylesheets/stylesheet.css\"/>"
                                         "<link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/stylesheets/syntax-highlight.css\"/>"
                                         ;; icon
                                         "<link ref=\"icon\" type=\"image/png\" href=\"/assets/images/favicon-16x16.png\" sizes=\"16x16\">"
                                         "<link ref=\"icon\" type=\"image/png\" href=\"/assets/images/favicon-32x32.png\" sizes=\"32x32\">"
                                         )
               ;; [ info.js]
               :html-use-infojs t
               ))

(add-to-list 'org-publish-project-alist
             '("Poem" :components ("poem-org" "poem-data")))

(use-package ox-reveal
  :ensure t
  :ensure htmlize
  :defer t
  :preface (setq org-reveal-note-key-char nil) ; avoid register old #+BEGIN_NOTES.
  :init (require 'ox-reveal))

(add-to-list 'org-publish-project-alist
             `("slides-data"
               :base-directory ,(concat my-org-publish-source "Slides/data")
               :base-extension any
               :publishing-directory ,(concat my-org-publish-destination "Slides/data")
               :recursive t
               :publishing-function org-publish-attachment))

(add-to-list 'org-publish-project-alist
             `("slides-org"
               :base-directory ,(concat my-org-publish-source "Slides")
               :base-extension "org"
               :publishing-directory ,(concat my-org-publish-destination "Slides")
               :publishing-function org-reveal-publish-to-reveal
               :recursive t
               :exclude-tags ("noexport" "note")
               :reveal-single-file t))

(add-to-list 'org-publish-project-alist
             '("Slides" :components ("slides-org" "slides-data")))

(add-to-list 'org-publish-project-alist
             '("WEBSITE"
               :components ("assets" "About" "Blog" "Poem" "Slides" "Index")))


(defun my-org-publish-finished-notify (args)
  "Notify user publish finished and pass in project property list `ARGS'."
  (require 'notifications)
  (notifications-notify :title "org-publish" :body "completed.")
  ;; TODO: use this plist
  ;; (print (plist-get args :base-extension))
  )

(defun my-org-publish-sync (args)
  "Sync ox-publish exported files to remote server and pass in project property list `ARGS'."
  (interactive)
  (let ((post (read-from-minibuffer "Post title: "))
        (cd-source (format "cd %s && " (concat org-directory "/Website")))
        (cd-master (format "cd %s && " my-org-publish-destination)))
    ;; source branch "~/Org/Website/"
    (or (zerop (shell-command (concat cd-source "git add . ")))
        (error "Source Branch: git-add error"))
    (or (zerop (shell-command (concat cd-source "git commit --message='" post "'")))
        (error "Source branch: git-commit error"))
    (or (zerop (shell-command (concat cd-source "git push origin source")))
        (error "Source branch: git-push to source error"))
    ;; gh-pages branch "~/org-publish/"
    (or (zerop (shell-command (concat cd-master "git add . ")))
        (error "Master branch: git-add error"))
    (or (zerop (shell-command (concat cd-master "git commit --message='" post "' ")))
        (error "Master branch: git-commit error"))
    (or (zerop (shell-command (concat cd-master "git push origin master")))
        (error "Master branch: git-push to master error"))
    ))

(defun my-org-publish-reset ()
  "Reset org-publish with DELETE all generated files."
  (interactive)
  (when (y-or-n-p "Are you seriously sure want to delete all org-publish files? ")
    (shell-command "rm -rf ~/.org-timestamps/")
    (shell-command "rm -rf ~/org-publish/*")))

(require 'org-capture)
(defun my/org-capture-template-blog--get-title ()
  (let ((title (read-from-minibuffer "Blog Title: ")))
    (format "* %s\n:PROPERTIES:\n:DATE: %%U\n:END: \n\n[[file:%s.org][%s]]\n%%i" title title title)))
(add-to-list
 'org-capture-templates
 '("b" "[b]log"
   entry (file "~/Org/Website/Blog/index.org")
   (function my/org-capture-template-blog--get-title)
   :empty-lines 1
   :prepend t
   :immediate-finish t
   :jump-to-captured t
   ))

(define-key Org-prefix (kbd "b") 'org-publish)



(provide 'init-org-publish)

;;; init-org-publish.el ends here
