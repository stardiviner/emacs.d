;;; init-org-publish.el --- init for Org Publish.

;;; Commentary:

;;; Code:

(use-package ox-publish
  :defer t
  :preface
  (defvar my-org-publish-source "~/Org/Website/")
  (defvar my-org-publish-destination "~/org-publish/")
  :bind (:map Org-prefix ("b" . org-publish))
  :init
  (use-package org-capture
    :init
    (defun my/org-capture-template-blog--generate-template ()
      (let ((title (read-from-minibuffer "Blog Title: ")))
        (format "* %s\n:PROPERTIES:\n:DATE: %%U\n:END:\n\n[[file:%s.org][%s]]\n%%i"
                title
                ;; make URL use - to replace space %20.
                (replace-regexp-in-string "\\ " "-" title)
                title)))
    (add-to-list 'org-capture-templates
                 `("B" ,(format "%s\tblog"
                                (all-the-icons-faicon "calendar-o" :face 'all-the-icons-blue))
                   ;; TODO: how to use pause on new capture for later refile to target file?
                   entry (file "~/Org/Website/Blog/index.org")
                   (function my/org-capture-template-blog--generate-template)
                   :empty-lines 1
                   :prepend t
                   :immediate-finish t
                   :jump-to-captured t)))
  :config
  (use-package ox-html
    :init
;;; Allow displaying source code block using "klipse".
    (setq org-html-klipsify-src nil)

    (setq org-html-allow-name-attribute-in-anchors t)

;;; org-info.js
    (setq org-html-infojs-options
          '((path . "/assets/scripts/org-info-src.js")
            (view . "showall") ; "info", "overview", "content", "showall"
            (toc . :with-toc) ; TOC visible?
            (ftoc . "2") ; which headline level to start hide fixed TOC?
            (ltoc . "3") ; show local TOC of each child headline section?
            (tdepth . "max") ; depth of TOC
            (sdepth . "max") ; maximum headline level
            (mouse . "underline") ; headlines is how highlighted when mouse is over them?
            (buttons . "0") ; should view-toggle buttons be everwhere?
            (up . :html-link-up)
            (home . :html-link-home))
          ;; org-export-html-use-infojs t
          ))

  (use-package ox-rss
    :commands (org-rss-publish-to-rss))

  ;; [ ox-org ]
  ;; support "Show Org Source" (.org.html version) button.
  ;; `org-org-publish-to-org'
  (use-package ox-org
    :ensure htmlize
    :config
    ;; - inline CSS (better colors support for src blocks)
    ;; use a specific theme for `ox-org'.
    (setq org-html-htmlize-output-type 'inline-css
          org-org-htmlized-css-url t)
    ;; FIXME: this seems does not work.
    (setq my:org-html-export-theme 'leuven)
    (defun my:ox-org-with-theme (orig-fun &rest args)
      (load-theme my:org-html-export-theme)
      (unwind-protect
          (apply orig-fun args)
        (disable-theme my:org-html-export-theme)))
    (advice-add 'org-export-to-buffer :around 'my:ox-org-with-theme))
  
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
           ;; [ Show Org source, generate .org.html ]
           :htmlized-source t ; toggle for `org-org-publish-to-org'
           ;; NOTE: conflict with customize font-lock: font-lock-add-keywords defined org faces.
           ;; [ src blocks ]
           :html-klipsify-src nil
           ;; [ images ]
           :html-inline-images t
           ;; [ stylesheet ]
           :html-head-include-default-style t
           :html-head-extra
           ,(concat
             ;; icon
             "<link ref=\"icon\" type=\"image/png\" href=\"/assets/images/favicon-16x16.png\" sizes=\"16x16\">"
             "<link ref=\"icon\" type=\"image/png\" href=\"/assets/images/favicon-32x32.png\" sizes=\"32x32\">"
             ;; Stylesheet
             "<link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/stylesheets/stylesheet.css\"/>"
             "<link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/stylesheets/syntax-highlight.css\"/>"
             ;; RSS
             "<link rel=\"alternate\" type=\"application/rss+xml\" href=\"https://stardiviner.github.io/Blog/index.xml\">"
             ;; org-info.js
             ;; "<script type=\"text/javascript\" language=\"JavaScript\" src=\"/assets/scripts/org-info-src.js\"></script>"
             ;; JQuery
             ;; "<script type=\"text/javascript\" src=\"http://ajax.googleapis.com/ajax/libs/jquery/3.4.1/jquery.min.js\"></script>"
             "<script type=\"text/javascript\" src=\"/assets/scripts/jquery-3.4.1.min.js\"></script>"
             ;; main.js
             "<script type=\"text/javascript\" src=\"/assets/scripts/main.js\"></script>"
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

  (add-to-list
   'org-publish-project-alist
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
     :section-numbers nil
     :with-toc nil
     :table-of-contents nil
     ;; Generate .org.html
     :htmlized-source t
     ;; [ images ]
     :html-inline-images t
     ;; [ stylesheet ]
     :html-head-include-default-style t
     :html-head-extra
     ,(concat
       ;; icon
       "<link ref=\"icon\" type=\"image/png\" href=\"/assets/images/favicon-16x16.png\" sizes=\"16x16\">"
       "<link ref=\"icon\" type=\"image/png\" href=\"/assets/images/favicon-32x32.png\" sizes=\"32x32\">"
       ;; Stylesheet
       "<link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/stylesheets/stylesheet.css\"/>"
       "<link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/stylesheets/syntax-highlight.css\"/>"
       ;; RSS
       "<link rel=\"alternate\" type=\"application/rss+xml\" href=\"https://stardiviner.github.io/Blog/index.xml\">"
       ;; org-info.js
       ;; "<script type=\"text/javascript\" language=\"JavaScript\" src=\"/assets/scripts/org-info-src.js\"></script>"
       ;; JQuery
       ;; "<script type=\"text/javascript\" src=\"http://ajax.googleapis.com/ajax/libs/jquery/3.4.1/jquery.min.js\"></script>"
       "<script type=\"text/javascript\" src=\"/assets/scripts/jquery-3.4.1.min.js\"></script>"
       ;; main.js
       "<script type=\"text/javascript\" src=\"/assets/scripts/main.js\"></script>"       
       ;; MathJax
       "<script type=\"text/javascript\" src=\"https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-AMS_HTML\"></script>"

       ;; tongji.baidu.com site statistics
       "<script>var _hmt = _hmt || [];(function() {var hm = document.createElement(\"script\");hm.src = \"https://hm.baidu.com/hm.js?67a64d34ee16392a25f7065993fa3aca\";var s = document.getElementsByTagName(\"script\")[0];s.parentNode.insertBefore(hm, s);})();</script>"
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
                 :section-numbers nil
                 :with-toc t
                 :table-of-contents nil
                 ;; Generate .org.html
                 :htmlized-source t
                 ;; [ images ]
                 :html-inline-images t
                 ;; [ stylesheet ]
                 :html-head-include-default-style t
                 :html-head-extra
                 ,(concat
                   ;; icon
                   "<link ref=\"icon\" type=\"image/png\" href=\"/assets/images/favicon-16x16.png\" sizes=\"16x16\">"
                   "<link ref=\"icon\" type=\"image/png\" href=\"/assets/images/favicon-32x32.png\" sizes=\"32x32\">"
                   ;; Stylesheet
                   "<link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/stylesheets/stylesheet.css\"/>"
                   "<link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/stylesheets/syntax-highlight.css\"/>"
                   ;; RSS
                   "<link rel=\"alternate\" type=\"application/rss+xml\" href=\"https://stardiviner.github.io/Blog/index.xml\">"
                   ;; org-info.js
                   ;; "<script type=\"text/javascript\" language=\"JavaScript\" src=\"/assets/scripts/org-info-src.js\"></script>"
                   ;; JQuery
                   ;; "<script type=\"text/javascript\" src=\"http://ajax.googleapis.com/ajax/libs/jquery/3.4.1/jquery.min.js\"></script>"
                   "<script type=\"text/javascript\" src=\"/assets/scripts/jquery-3.4.1.min.js\"></script>"
                   ;; main.js
                   "<script type=\"text/javascript\" src=\"/assets/scripts/main.js\"></script>"       
                   ;; MathJax
                   "<script type=\"text/javascript\" src=\"https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-AMS_HTML\"></script>"
                   )
                 ;; [ info.js]
                 :html-use-infojs t
                 ))

  (add-to-list 'org-publish-project-alist
               '("About" :components ("about-org" "about-data")))

  (add-to-list 'org-publish-project-alist
               `("thought-data"
                 :base-directory ,(concat my-org-publish-source "Thought/data")
                 :base-extension any
                 :publishing-directory ,(concat my-org-publish-destination "Thought/data")
                 :recursive t
                 :publishing-function org-publish-attachment))

  (add-to-list 'org-publish-project-alist
               `("thought-org"
                 :base-directory ,(concat my-org-publish-source "Thought")
                 :base-extension "org"
                 :recursive t
                 :publishing-directory ,(concat my-org-publish-destination "Thought")
                 :publishing-function (org-html-publish-to-html)
                 
                 ;; [ html ]
                 :html-doctype "html5"
                 :html-html5-fancy t ; use new HTML5 elements.
                 :html-head-include-scripts t
                 :html-link-home "https://stardiviner.github.io/"
                 :html-preamble ,(org-file-contents (concat my-org-publish-source "assets/preamble.html"))
                 :html-postamble ,(org-file-contents (concat my-org-publish-source "assets/postamble.html"))
                 :html-link-org-files-as-html t
                 :section-numbers nil
                 :with-toc t
                 :table-of-contents nil
                 ;; Generate .org.html
                 :htmlized-source t
                 ;; [ images ]
                 :html-inline-images t
                 ;; [ stylesheet ]
                 :html-head-include-default-style t
                 :html-head-extra
                 ,(concat
                   ;; icon
                   "<link ref=\"icon\" type=\"image/png\" href=\"/assets/images/favicon-16x16.png\" sizes=\"16x16\">"
                   "<link ref=\"icon\" type=\"image/png\" href=\"/assets/images/favicon-32x32.png\" sizes=\"32x32\">"
                   ;; Stylesheet
                   "<link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/stylesheets/stylesheet.css\"/>"
                   "<link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/stylesheets/syntax-highlight.css\"/>"
                   ;; RSS
                   "<link rel=\"alternate\" type=\"application/rss+xml\" href=\"https://stardiviner.github.io/Blog/index.xml\">"
                   ;; org-info.js
                   ;; "<script type=\"text/javascript\" language=\"JavaScript\" src=\"/assets/scripts/org-info-src.js\"></script>"
                   ;; JQuery
                   ;; "<script type=\"text/javascript\" src=\"http://ajax.googleapis.com/ajax/libs/jquery/3.4.1/jquery.min.js\"></script>"
                   "<script type=\"text/javascript\" src=\"/assets/scripts/jquery-3.4.1.min.js\"></script>"
                   ;; main.js
                   "<script type=\"text/javascript\" src=\"/assets/scripts/main.js\"></script>"
                   )
                 ;; [ info.js]
                 :html-use-infojs t
                 ))

  (add-to-list 'org-publish-project-alist
               '("Thought" :components ("thought-org" "thought-data")))

  (add-to-list 'org-publish-project-alist
               `("poem-data"
                 :base-directory ,(concat my-org-publish-source "Literature/Poem/data")
                 :base-extension any
                 :publishing-directory ,(concat my-org-publish-destination "Literature/Poem/data")
                 :recursive t
                 :publishing-function org-publish-attachment))

  (add-to-list 'org-publish-project-alist
               `("poem-org"
                 :base-directory ,(concat my-org-publish-source "Literature/Poem")
                 :base-extension "org"
                 :recursive t
                 :publishing-directory ,(concat my-org-publish-destination "Literature/Poem")
                 :publishing-function org-html-publish-to-html
                 
                 ;; [ html ]
                 :html-doctype "html5"
                 :html-html5-fancy t ; use new HTML5 elements.
                 :html-head-include-scripts t
                 :html-link-home "https://stardiviner.github.io/"
                 :html-preamble ,(org-file-contents (concat my-org-publish-source "assets/preamble.html"))
                 :html-postamble ,(org-file-contents (concat my-org-publish-source "assets/postamble.html"))
                 :html-link-org-files-as-html t
                 :section-numbers nil
                 :with-toc t
                 :table-of-contents nil
                 ;; Generate .org.html
                 :htmlized-source t
                 ;; [ images ]
                 :html-inline-images t
                 ;; [ stylesheet ]
                 :html-head-include-default-style t
                 :html-head-extra
                 ,(concat
                   ;; icon
                   "<link ref=\"icon\" type=\"image/png\" href=\"/assets/images/favicon-16x16.png\" sizes=\"16x16\">"
                   "<link ref=\"icon\" type=\"image/png\" href=\"/assets/images/favicon-32x32.png\" sizes=\"32x32\">"
                   ;; Stylesheet
                   "<link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/stylesheets/stylesheet.css\"/>"
                   "<link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/stylesheets/syntax-highlight.css\"/>"
                   ;; RSS
                   "<link rel=\"alternate\" type=\"application/rss+xml\" href=\"https://stardiviner.github.io/Blog/index.xml\">"
                   ;; org-info.js
                   ;; "<script type=\"text/javascript\" language=\"JavaScript\" src=\"/assets/scripts/org-info-src.js\"></script>"
                   ;; JQuery
                   ;; "<script type=\"text/javascript\" src=\"http://ajax.googleapis.com/ajax/libs/jquery/3.4.1/jquery.min.js\"></script>"
                   "<script type=\"text/javascript\" src=\"/assets/scripts/jquery-3.4.1.min.js\"></script>"
                   ;; main.js
                   "<script type=\"text/javascript\" src=\"/assets/scripts/main.js\"></script>"
                   )
                 ;; [ info.js]
                 :html-use-infojs t
                 ))

  (add-to-list 'org-publish-project-alist
               '("Poem" :components ("poem-org" "poem-data")))

  (add-to-list 'org-publish-project-alist
               `("novel-data"
                 :base-directory ,(concat my-org-publish-source "Literature/Novel/data")
                 :base-extension any
                 :publishing-directory ,(concat my-org-publish-destination "Literature/Novel/data")
                 :recursive t
                 :publishing-function org-publish-attachment))

  (add-to-list 'org-publish-project-alist
               `("novel-org"
                 :base-directory ,(concat my-org-publish-source "Literature/Novel")
                 :base-extension "org"
                 :recursive t
                 :publishing-directory ,(concat my-org-publish-destination "Literature/Novel")
                 :publishing-function org-html-publish-to-html
                 
                 ;; [ html ]
                 :html-doctype "html5"
                 :html-html5-fancy t ; use new HTML5 elements.
                 :html-head-include-scripts t
                 :html-link-home "https://stardiviner.github.io/"
                 :html-preamble ,(org-file-contents (concat my-org-publish-source "assets/preamble.html"))
                 :html-postamble ,(org-file-contents (concat my-org-publish-source "assets/postamble.html"))
                 :html-link-org-files-as-html t
                 :section-numbers nil
                 :with-toc t
                 :table-of-contents nil
                 ;; Generate .org.html
                 :htmlized-source t
                 ;; [ images ]
                 :html-inline-images t
                 ;; [ stylesheet ]
                 :html-head-include-default-style t
                 :html-head-extra
                 ,(concat
                   ;; icon
                   "<link ref=\"icon\" type=\"image/png\" href=\"/assets/images/favicon-16x16.png\" sizes=\"16x16\">"
                   "<link ref=\"icon\" type=\"image/png\" href=\"/assets/images/favicon-32x32.png\" sizes=\"32x32\">"
                   ;; Stylesheet
                   "<link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/stylesheets/stylesheet.css\"/>"
                   "<link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/stylesheets/syntax-highlight.css\"/>"
                   ;; RSS
                   "<link rel=\"alternate\" type=\"application/rss+xml\" href=\"https://stardiviner.github.io/Blog/index.xml\">"
                   ;; org-info.js
                   ;; "<script type=\"text/javascript\" language=\"JavaScript\" src=\"/assets/scripts/org-info-src.js\"></script>"
                   ;; JQuery
                   ;; "<script type=\"text/javascript\" src=\"http://ajax.googleapis.com/ajax/libs/jquery/3.4.1/jquery.min.js\"></script>"
                   "<script type=\"text/javascript\" src=\"/assets/scripts/jquery-3.4.1.min.js\"></script>"
                   ;; main.js
                   "<script type=\"text/javascript\" src=\"/assets/scripts/main.js\"></script>"
                   )
                 ;; [ info.js]
                 :html-use-infojs t
                 ))

  (add-to-list 'org-publish-project-alist
               '("Novel" :components ("novel-org" "novel-data")))

  (add-to-list 'org-publish-project-alist
               `("literature-data"
                 :base-directory ,(concat my-org-publish-source "Literature/data")
                 :base-extension any
                 :publishing-directory ,(concat my-org-publish-destination "Literature/data")
                 :recursive t
                 :publishing-function org-publish-attachment))

  (add-to-list 'org-publish-project-alist
               `("literature-org"
                 :base-directory ,(concat my-org-publish-source "Literature")
                 :base-extension "org"
                 :recursive t
                 :publishing-directory ,(concat my-org-publish-destination "Literature")
                 :publishing-function org-html-publish-to-html
                 
                 ;; [ html ]
                 :html-doctype "html5"
                 :html-html5-fancy t ; use new HTML5 elements.
                 :html-head-include-scripts t
                 :html-link-home "https://stardiviner.github.io/"
                 :html-preamble ,(org-file-contents (concat my-org-publish-source "assets/preamble.html"))
                 :html-postamble ,(org-file-contents (concat my-org-publish-source "assets/postamble.html"))
                 :html-link-org-files-as-html t
                 :section-numbers nil
                 :with-toc t
                 :table-of-contents nil
                 ;; Generate .org.html
                 :htmlized-source t
                 ;; [ images ]
                 :html-inline-images t
                 ;; [ stylesheet ]
                 :html-head-include-default-style t
                 :html-head-extra
                 ,(concat
                   ;; icon
                   "<link ref=\"icon\" type=\"image/png\" href=\"/assets/images/favicon-16x16.png\" sizes=\"16x16\">"
                   "<link ref=\"icon\" type=\"image/png\" href=\"/assets/images/favicon-32x32.png\" sizes=\"32x32\">"
                   ;; Stylesheet
                   "<link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/stylesheets/stylesheet.css\"/>"
                   "<link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/stylesheets/syntax-highlight.css\"/>"
                   ;; RSS
                   "<link rel=\"alternate\" type=\"application/rss+xml\" href=\"https://stardiviner.github.io/Blog/index.xml\">"
                   ;; org-info.js
                   ;; "<script type=\"text/javascript\" language=\"JavaScript\" src=\"/assets/scripts/org-info-src.js\"></script>"
                   ;; JQuery
                   ;; "<script type=\"text/javascript\" src=\"http://ajax.googleapis.com/ajax/libs/jquery/3.4.1/jquery.min.js\"></script>"
                   "<script type=\"text/javascript\" src=\"/assets/scripts/jquery-3.4.1.min.js\"></script>"
                   ;; main.js
                   "<script type=\"text/javascript\" src=\"/assets/scripts/main.js\"></script>"
                   )
                 ;; [ info.js]
                 :html-use-infojs t
                 ))

  (add-to-list 'org-publish-project-alist
               '("Literature" :components ("literature-data" "literature-org" "Poem" "Novel")))

  (add-to-list 'org-publish-project-alist
               '("WEBSITE"
                 :components ("assets" "About" "Blog" "Thought" "Literature" "Index")))

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
    (require 'init-org-publish) ; reload org-publish init file to refresh new settings.
    (when (y-or-n-p "Are you seriously sure want to delete all org-publish files? ")
      (shell-command "rm -rf ~/.org-timestamps/")
      (shell-command "rm -rf ~/org-publish/*")))
  )


(use-package ox-reveal
  :ensure t
  :ensure htmlize
  :defer t
  :preface (setq org-reveal-note-key-char nil) ; avoid register old #+BEGIN_NOTES.
  :init (setq org-reveal-root "./reveal.js")
  :config
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
               '("Slides" :components ("slides-org" "slides-data"))))



(provide 'init-org-publish)

;;; init-org-publish.el ends here
