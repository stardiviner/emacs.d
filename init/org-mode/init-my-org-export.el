;;; init-my-org-export.el --- init for Org Exporting
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(setq org-export-backends '(ascii html icalendar latex md)
      org-export-headline-levels 5
      org-export-with-drawers t
      org-export-with-emphasize t
      org-export-babel-evaluate nil
      org-export-with-smart-quotes t
      )


;;; ------------- Export UTF-8 checkboxes ---------------------
;;; This snippet turns - [X] into ☑ and - [ ] into ☐.

;; 'ascii, 'unicode, 'html
(setq org-html-checkbox-type 'ascii)


;;; ox-html
(require 'ox-html)

(setq org-html-doctype "html5"
      org-html-html5-fancy t
      org-html-use-infojs t
      ;; org-html-infojs-options
      org-html-indent nil ; t: may break code blocks.
      ;; org-html-format-drawer-function
      ;; org-html-footnotes-section
      org-html-toplevel-hlevel 1 ; use <h1> for h1 level headlines.
      ;; org-html-format-headline-function
      org-html-allow-name-attribute-in-anchors t
      ;; org-html-format-inlinetask-function
      org-html-with-latex t ; t: mathjax, 'divpng, 'imagemagick, 'verbatim,
      org-html-inline-images t
      org-html-htmlize-output-type 'inline-css ; make HTML self-containing
      org-html-htmlize-font-prefix "org-"
      
      ;; org-html-table-default-attributes
      ;; '(:border "2" :cellspacing "0" :cellpadding "6"
      ;;           :rules "groups" :frame "hsides")
      ;; org-html-table-header-tags '("<th scope=\"%s\"%s>" . "</th>")
      ;; org-html-table-data-tags '("<td%s>" . "</td>")
      ;; org-html-table-row-tags '("<tr>" . "</tr>")
      org-html-table-align-individual-fields t
      org-html-table-caption-above t
      
      ;; org-html-tag-class-prefix ""
      
      org-html-coding-system 'utf-8
      
      ;; org-html-mathjax-options
      ;; '((path
      ;;   "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML")
      ;;   (scale "100")
      ;;   (align "center")
      ;;   (font "TeX")
      ;;   (linebreaks "false")
      ;;   (autonumber "AMS")
      ;;   (indent "0em")
      ;;   (multlinewidth "85%")
      ;;   (tagindent ".8em")
      ;;   (tagside "right")
      ;;   )
      ;; org-html-mathjax-template
      
      org-html-postamble 't ; author, email, creator, date,
      ;;       org-html-postamble-format
      ;;       '(("en" "<p class=\"author\">Author: %a (%e)</p>
      ;; <p class=\"date\">Date: %d</p>
      ;; <p class=\"creator\">%c</p>
      ;; <p class=\"validation\">%v</p>"))
      ;;
      org-html-preamble t
      ;; org-html-preamble-format

      ;; org-html-link-home
      ;; org-html-link-up
      org-html-link-use-abs-url nil
      org-html-link-org-files-as-html t
      ;; org-html-home/up-format

      ;; org-html-head ""
      ;; org-html-head-extra ""

      ;; org-html-viewport

      ;; org-html-todo-kwd-class-prefix ""
      )


;;; [ ox-pandoc ] -- another org-mode exporter via pandoc.

(use-package ox-pandoc
  :ensure t)


;;; export to LaTeX

;; syntax highlight in LaTeX output (Minted)
(require 'ox-latex)


;;; convert selected region to Markdown and copy to clipboard for pasting
;;; on sites like GitHub, and Stack Overflow.

(unless (boundp 'paste-prefix)
  (define-prefix-command 'paste-prefix))
(define-key paste-prefix (kbd "m") 'my-org-md-convert-region-to-md)

(defun my-org-md-convert-region-to-md ()
  "convert selected region to Markdown and copy to clipboard for
pasting on sites like GitHub, and Stack Overflow."
  (interactive)
  (unless (org-region-active-p) (user-error "No active region to replace"))
  (x-set-selection 'CLIPBOARD
                   (org-export-string-as
                    (buffer-substring (region-beginning) (region-end)) 'md t)))


;;; email org-mode region/buffer

;; send email with org-mode region as message.
;; I like to email org-mode headings and content to people. It would be nice to
;; have some records of when a heading was sent, and to whom. We store this
;; information in a heading. It is pretty easy to write a simple function that
;; emails a selected region.
;;
;; use `org-mime'
;; - `org-mime-org-buffer-htmlize' ::

(require 'org-mime)

(add-hook 'message-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c M-o") 'org-mime-htmlize)))
(add-hook 'org-mode-hook
          '(lambda ()
             ;; `org-mime-org-buffer-htmlize', `org-mime-htmlize'
             (local-set-key (kbd "C-x M") 'org-mime-org-buffer-htmlize)

             ;; change element style
             (org-mime-change-element-style
              "pre" (format "color: %s; background-color: %s; padding: 0.5em;"
                            "#E6E1DC" "#232323"))
             (org-mime-change-element-style
              "blockquote" "border-left: 2px solid gray; padding-left: 4px;")
             ))


;;; [ ox-publish ]

(require 'ox-publish)

(setq org-publish-use-timestamps-flag t)

(setq org-html-use-infojs nil)

(setq org-publish-project-alist
      '(("Blog"
         :base-directory "~/Org/Blog/org-publish/Blog/"
         :base-extension any ; "org"
         :publishing-directory "~/Org/Blog/org-publish/exported_html/Blog"
         ;; publish to remote with Tramp.
         ;; :publishing-directory "/ssh:user@host#port:/path/to/dir"
         :recursive t
         :publishing-function org-html-publish-to-html
         
         :html-link-home "http://stardiviner.github.io/"
         :html-head-extra "<link rel=\"stylesheet\" href=\"assets/stylesheets/stylesheet.css\" type=\"text/css\"/> <link rel=\"alternate\" type=\"application/rss+xml\" href=\"http://stardiviner.github.io/sitemap.xml\" title=\"RSS feed\">"
         )

        ("Blog-RSS"
         :base-directory "~/Org/Blog/org-publish/Blog/"
         :base-extension "org"
         :publishing-directory "~/Org/Blog/org-publish/exported_html/Blog-RSS"
         :recursive t
         ;; todo: replace with my domain
         :html-link-home "http://stardiviner.github.io/"
         :html-link-use-abs-url t
         :publishing-function org-rss-publish-to-rss
         :completion-function (my-ox-publish-complete-notify)
         )

        ;; ("Images"
        ;;  :base-directory "~/Org/Blog/org-publish/images"
        ;;  :base-extension "jpg\\|png\\|gif"
        ;;  :publishing-directory "/ssh:user@host#port:/path/to/dir"
        ;;  :publishing-function org-publish-attachment)

        ;; ("Wiki"
        ;;  :base-directory "~/Org"
        ;;  :base-extension any
        ;;  :recursive t
        ;;  :publishing-directory "~/Org/Blog/org-publish/exported_html/Wiki/"
        ;;  ;; publish to remote with Tramp.
        ;;  ;; :publishing-directory "/ssh:user@host#port:/path/to/dir"
        ;;  :publishing-function org-html-publish-to-html
        ;;  )
        
        ("website"
         :components ("Blog" "Blog-RSS")
         :publishing-directory "~/Org/Blog/org-publish/exported_html/"
         :publishing-function org-html-publish-to-html
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
         :html-head-extra "<link rel=\"stylesheet\" href=\"assets/stylesheets/stylesheet.css\" type=\"text/css\"/>"
         :html-preamble t
         :html-postamble t
         :html-link-home t
         :html-link-up t
         :html-html5-fancy t
         :html-inline-images t
         :section-numbers t
         :with-toc t
         ;; :htmlized-source t
         ;; [ sitemap & index ]
         :auto-sitemap t
         :sitemap-title "stardiviner's site"
         :makeindex t
         )
        ))

(add-to-list
 'org-capture-templates
 '("B" "Blog"
   entry (file+datetree "~/Org/Blog/org-publish/source/index.org")
   "\n* %^{blog title}\n:PROPERTIES:\n:TIME: %U\n:END: \n\n[[file:%<%Y-%m-%d %R>.org][%^{blog title}]]\n\n%i"
   :empty-lines 1
   :jump-to-captured t
   ))

(defun my-ox-publish-complete-notify ()
  (notifications-notify :title "ox-publish" :body "completed."))


;;; [ RSS ]

(require 'ox-rss)


;;; [ Beamer ]

(require 'ox-beamer)


;;; [ Mindmap ]

(require 'ox-freemind)


(provide 'init-my-org-export)

;;; init-my-org-export.el ends here
