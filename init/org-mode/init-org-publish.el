;;; init-org-publish.el --- init for Org Publish.

;;; Commentary:

;;; Code:

(require 'ox-publish)
(require 'ox-html)
(require 'ox-rss)

(setq org-html-htmlize-output-type 'css)

(defvar my-org-publish-directory "~/org-publish/")

;; projects definition
(setq org-publish-project-alist
      `(("blog-org"
         :base-directory ,(concat org-directory "/Website/Blog/")
         :base-extension "org"
         :recursive t
         :exclude-tags ("noexport" "todo")
         :publishing-function (org-html-publish-to-html org-org-publish-to-org)
         :publishing-directory ,(concat my-org-publish-directory "Blog/")

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
         :html-preamble
         "<div class='nav'>
         <ul>
         <li><a href='/'>Home</a></li>
         <li><a href='/Blog/index.html'>Blog</a></li>
         <li><a href='/Poem/index.html'>Poem</a></li>
         <li><a href='/About/index.html'>About</a></li>
         </ul>
         </div>"
         :html-postamble
         "<div class='footer'>
         Copyright 2011, Author: %a (%e).<br>
         Last update date: %C.<br>
         Built with %c. (%v HTML)
         </div>

<div class='reward'>
<div id='alipay-payment'><a href=\"/About/index.html#Payment\"><img src=\"/assets/images/alipay_payment.jpg\" title=\"打赏\" /></a></div>
<div id='wechat-payment'><a href=\"/About/index.html#Payment\"><img src=\"/assets/images/wechat_payment.png\" title=\"打赏\" /></a></div>
</div>

<div id=\"disqus_thread\"></div>
<script>
(function() {
var d = document, s = d.createElement('script');
s.src = 'https://stardiviner.disqus.com/embed.js';
s.setAttribute('data-timestamp', +new Date());
(d.head || d.body).appendChild(s);
})();
</script>
"
         :html-link-org-files-as-html t
         :section-numbers t
         :with-toc nil
         :table-of-contents nil
         ;; [ src code block ]
         :htmlized-source nil ; src code block syntax highlighting
         ;; use external CSS stylesheet instead.
         ;; NOTE: conflict with customize font-lock: font-lock-add-keywords defined org faces.
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
         :base-directory ,(concat org-directory "/Website/Blog/")
         :base-extension "org"
         :publishing-directory ,(concat my-org-publish-directory "Blog")
         :publishing-function (org-rss-publish-to-rss) ; generate index.xml
         ;; :rss-image-url "https://stardiviner.github.io/assets/images/RSS.png"
         :rss-extension "xml"
         :html-link-home "https://stardiviner.github.io/"
         :html-link-use-abs-url t
         :exclude ".*" ; exclude all other files...
         :include ("index.org") ; ... except index.org.
         :section-numbers nil
         :table-of-contents nil)

        ("blog-data-code"
         :base-directory ,(concat org-directory "/Website/Blog/data/code")
         :base-extension any
         :publishing-directory ,(concat my-org-publish-directory "Blog/data/code")
         :recursive t
         :publishing-function org-publish-attachment)
        ("blog-data-images"
         :base-directory ,(concat org-directory "/Website/Blog/data/images")
         :base-extension any
         :publishing-directory ,(concat my-org-publish-directory "Blog/data/images")
         :recursive t
         :publishing-function org-publish-attachment)
        ("blog-data-docs"
         :base-directory ,(concat org-directory "/Website/Blog/data/docs")
         :base-extension any
         :publishing-directory ,(concat my-org-publish-directory "Blog/data/docs")
         :recursive t
         :publishing-function org-publish-attachment)
        ("blog-data-videos"
         :base-directory ,(concat org-directory "/Website/Blog/data/videos")
         :base-extension any
         :publishing-directory ,(concat my-org-publish-directory "Blog/data/videos")
         :recursive t
         :publishing-function org-publish-attachment)
        ))

(add-to-list 'org-publish-project-alist
             '("Blog"
               :components ("blog-org"
                            "blog-RSS"
                            "blog-data-code" "blog-data-images" "blog-data-docs" "blog-data-videos")))

(add-to-list 'org-publish-project-alist
             `("assets"                  ; website materials: "JS", "CSS", "Images" etc.
               :base-directory ,(concat org-directory "/Website/assets")
               :base-extension any
               :publishing-directory ,(concat my-org-publish-directory "assets/")
               :recursive t
               :publishing-function org-publish-attachment))

(add-to-list 'org-publish-project-alist
             `("Index"
               :base-directory ,(concat org-directory "/Website")
               :base-extension "org"
               :exclude ".*" ; exclude all other files...
               :include ("index.org") ; ... except index.org.
               :recursive nil
               :publishing-directory ,(expand-file-name my-org-publish-directory)
               :publishing-function org-html-publish-to-html
               
               ;; [ html ]
               :html-doctype "html5"
               :html-html5-fancy t ; use new HTML5 elements.
               :html-head-include-scripts t
               :html-link-home "https://stardiviner.github.io/"
               :html-preamble
               "<div class='nav'>
               <ul>
               <li><a href='/'>Home</a></li>
               <li><a href='/Blog/index.html'>Blog</a></li>
               <li><a href='/Poem/index.html'>Poem</a></li>
               <li><a href='/About/index.html'>About</a></li>
               </ul>
               </div>"
               :html-postamble
               "<div class='footer'>
         Copyright 2011, Author: %a (%e).<br>
         Last update date: %C.<br>
         Built with %c. (%v HTML)
         </div>"
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
             `("About"
               :base-directory ,(concat org-directory "/Website/About")
               :base-extension "org"
               :recursive t
               :publishing-directory ,(concat my-org-publish-directory "About")
               :publishing-function (org-html-publish-to-html)

               ;; [ html ]
               :html-doctype "html5"
               :html-html5-fancy t ; use new HTML5 elements.
               :html-head-include-scripts t
               :html-link-home "https://stardiviner.github.io/"
               :html-preamble
               "<div class='nav'>
               <ul>
               <li><a href='/'>Home</a></li>
               <li><a href='/Blog/index.html'>Blog</a></li>
               <li><a href='/Poem/index.html'>Poem</a></li>
               <li><a href='/About/index.html'>About</a></li>
               </ul>
               </div>"
               :html-postamble
               "<div class='footer'>
         Copyright 2011, Author: %a (%e).<br>
         Last update date: %C.<br>
         Built with %c. (%v HTML)
         </div>"
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

               :completion-function (my-org-publish-sync my-org-publish-finished-notify)
               ))

(add-to-list 'org-publish-project-alist
             `("about-data"                  ; website materials: "JS", "CSS", "Images" etc.
               :base-directory ,(concat org-directory "/Website/About")
               :base-extension any
               :publishing-directory ,(concat my-org-publish-directory "About/")
               :recursive t
               :publishing-function org-publish-attachment))

(add-to-list 'org-publish-project-alist
             `("Poem"
               :base-directory ,(concat org-directory "/Website/Poem")
               :base-extension "org"
               :recursive t
               :publishing-directory ,(concat my-org-publish-directory "Poem")
               :publishing-function org-html-publish-to-html
               
               ;; [ html ]
               :html-doctype "html5"
               :html-html5-fancy t ; use new HTML5 elements.
               :html-head-include-scripts t
               :html-link-home "https://stardiviner.github.io/"
               :html-preamble
               "<div class='nav'>
               <ul>
               <li><a href='/'>Home</a></li>
               <li><a href='/Blog/index.html'>Blog</a></li>
               <li><a href='/Poem/index.html'>Poem</a></li>
               <li><a href='/About/index.html'>About</a></li>
               </ul>
               </div>"
               :html-postamble
               "<div class='footer'>
         Copyright 2011, Author: %a (%e).<br>
         Last update date: %C.<br>
         Built with %c. (%v HTML)
         </div>

<div class='reward'>
<div id='alipay-payment'><a href=\"/About/index.html#Payment\"><img src=\"/assets/images/alipay_payment.jpg\" title=\"打赏\" /></a></div>
<div id='wechat-payment'><a href=\"/About/index.html#Payment\"><img src=\"/assets/images/wechat_payment.png\" title=\"打赏\" /></a></div>
</div>

<div id=\"disqus_thread\"></div>
<script>
(function() {
var d = document, s = d.createElement('script');
s.src = 'https://stardiviner.disqus.com/embed.js';
s.setAttribute('data-timestamp', +new Date());
(d.head || d.body).appendChild(s);
})();
</script>
"
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
             '("WEBSITE"
               :components ("assets" "Index" "Blog" "Poem" "about-data" "About")))


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
        (cd-master (format "cd %s && " my-org-publish-directory)))
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
(add-to-list
 'org-capture-templates
 '("b" "[B]log"
   entry (file+olp+datetree (concat org-directory "/Website/Blog/index.org"))
   "\n* %^{blog title}\n:PROPERTIES:\n:TIME: %U\n:END: \n\n[[file:%<%Y-%m-%d %R>.org][%^{blog title}]]\n\n%i"
   :empty-lines 1
   :jump-to-captured t
   ))

(define-key Org-prefix (kbd "b") 'org-publish)



(provide 'init-org-publish)

;;; init-org-publish.el ends here
