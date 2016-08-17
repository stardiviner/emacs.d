;;; init-my-org-export.el --- init for Org Exporting
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(setq org-export-backends '(ascii html icalendar latex md)
      org-export-headline-levels 5
      org-export-with-toc t
      org-export-with-tasks t
      org-export-with-section-numbers t
      org-export-with-todo-keywords t
      org-export-with-priority t
      org-export-with-clocks t
      org-export-with-planning t
      org-export-with-timestamps t
      org-export-with-tags t
      org-export-with-drawers t
      org-export-with-properties t
      org-export-with-footnotes t
      org-export-with-tables t
      org-export-with-latex t
      org-export-with-emphasize t
      org-export-babel-evaluate 'inline-only
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
      org-html-use-infojs 'when-configured
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
;; - `org-mime-org-buffer-htmlize' :: [C-x M]

(require 'org-mime)

(add-hook 'message-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c M-o") 'org-mime-htmlize)))
(add-hook 'org-mode-hook
          '(lambda ()
             ;; `org-mime-org-buffer-htmlize', `org-mime-htmlize'
             (local-set-key (kbd "C-x M") 'org-mime-org-buffer-htmlize)
             ))

(defun org-export-settings-for-email ()
  "Settings for export Org-mode buffer to HTML Email format."
  (interactive)

  ;; change element style
  (org-mime-change-element-style
   "pre" (format "color: %s; background-color: %s; padding: 0.5em;"
                 "#E6E1DC" "#232323"))
  (org-mime-change-element-style
   "blockquote" "border-left: 2px solid gray; padding-left: 4px;")
  )

;; FIXME: wrong number of arguments, 1.
;; (add-hook 'org-export-before-processing-hook 'org-export-settings-for-email)


;; ;;; [ ox-publish ]
;;
;; (require 'ox-publish)
;;
;; (setq my-org-publish-directory (expand-file-name "~/Org/Blog/org-publish"))
;;
;; ;; export files update style
;; (setq org-publish-use-timestamps-flag t)
;;
;; ;; link
;; (setq org-html-link-home "http://stardiviner.github.io/")
;;
;; ;; org-info.js
;; ;; (setq org-html-use-infojs t)
;;
;; ;; src code block syntax highlighting
;; (use-package htmlize
;;   :ensure t)
;;
;; (setq org-html-htmlize-output-type 'css
;;       org-html-htmlize-font-prefix "org-")
;;
;; ;; MathJax.js
;; (add-to-list
;;  'org-html-mathjax-options
;;  '(path
;;    "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML"))
;;
;; ;; projects definition
;; (setq org-publish-project-alist
;;       '(("Blog"
;;          :base-directory "~/Org/Blog/org-publish/Blog/"
;;          :base-extension "org"
;;          :publishing-directory "~/Org/Blog/org-publish/exported_html/Blog"
;;          ;; publish to remote with Tramp.
;;          ;; :publishing-directory "/ssh:user@host#port:/path/to/dir"
;;          ;; :remote (git "https://github.com/stardiviner/stardiviner.github.com.git" "master")
;;          :recursive t
;;          :publishing-function org-html-publish-to-html
;;
;;          :html-link-home "http://stardiviner.github.io/"
;;          :html-head-extra "<link rel=\"stylesheet\" href=\"assets/stylesheets/stylesheet.css\" type=\"text/css\"/> <link rel=\"alternate\" type=\"application/rss+xml\" href=\"http://stardiviner.github.io/sitemap.xml\" title=\"RSS feed\">"
;;          )
;;
;;         ("Blog-static"
;;          :base-directory "~/Org/Blog/org-publish/Blog/"
;;          :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
;;          :publishing-directory "~/Org/Blog/org-publish/exported_html/Blog"
;;          :recursive t
;;          :publishing-function org-publish-attachment
;;          )
;;
;;         ;; ("Blog-RSS"
;;         ;;  :base-directory "~/Org/Blog/org-publish/Blog/"
;;         ;;  :base-extension "org"
;;         ;;  :publishing-directory "~/Org/Blog/org-publish/exported_html/Blog-RSS"
;;         ;;  :recursive t
;;         ;;  :html-link-home "http://stardiviner.github.io/"
;;         ;;  :html-link-use-abs-url t
;;         ;;  :publishing-function org-rss-publish-to-rss
;;         ;;  :completion-function (my-ox-publish-complete-notify)
;;         ;;  )
;;
;;         ;; ("Wiki"
;;         ;;  :base-directory "~/Org"
;;         ;;  :base-extension any
;;         ;;  :recursive t
;;         ;;  :publishing-directory "~/Org/Blog/org-publish/exported_html/Wiki/"
;;         ;;  ;; publish to remote with Tramp.
;;         ;;  ;; :publishing-directory "/ssh:user@host#port:/path/to/dir"
;;         ;;  :publishing-function org-html-publish-to-html
;;         ;;  )
;;
;;         ("website"
;;          :components ("Blog" "Blog-static")
;;          ;; todo: :exclude ("exported_html/")
;;          :publishing-directory "~/Org/Blog/org-publish/exported_html/"
;;          :publishing-function org-html-publish-to-html
;;          :completion-function (my-ox-publish-complete-notify
;;                                my-ox-publish-sync)
;;          ;; [ author info ]
;;          :with-author "stardiviner"
;;          :with-email "numbchild@[gmail]"
;;          :with-footnotes "get over the world!"
;;          ;; [ latex ]
;;          :with-sub-superscript t
;;          :with-tables t
;;          :with-tags t
;;          ;; [ tasks ]
;;          :with-tasks t
;;          :with-todo-keywords t
;;          :with-planning t
;;          :with-timestamps t
;;          ;; [ html ]
;;          :html-doctype "html5"
;;          :html-head-include-default-style t
;;          :html-head-include-scripts t
;;          ;; :html-head
;;          ;; :html-head-extra "<link rel=\"stylesheet\" href=\"assets/stylesheets/stylesheet.css\" type=\"text/css\"/>"
;;          :html-preamble t
;;          :html-postamble t
;;          :html-link-home t
;;          :html-link-up t
;;          :html-html5-fancy t
;;          :html-inline-images t
;;          :section-numbers t
;;          :with-toc t
;;          ;; src code block syntax highlighting
;;          :htmlized-source t
;;          ;; [ stylesheet ]
;;          :style "<link rel=\"stylesheet\" href=\"assets/stylesheets/stylesheet.css\" type=\"text/css\"/>"
;;          ;; [ sitemap & index ]
;;          :auto-sitemap t
;;          :sitemap-title "stardiviner's site"
;;          :makeindex t
;;          )
;;         ))
;;
;; (add-to-list
;;  'org-capture-templates
;;  '("B" "Blog"
;;    entry (file+datetree "~/Org/Blog/org-publish/source/index.org")
;;    "\n* %^{blog title}\n:PROPERTIES:\n:TIME: %U\n:END: \n\n[[file:%<%Y-%m-%d %R>.org][%^{blog title}]]\n\n%i"
;;    :empty-lines 1
;;    :jump-to-captured t
;;    ))
;;
;; (defun my-ox-publish-complete-notify ()
;;   (notifications-notify :title "ox-publish" :body "completed."))
;;
;; (defun my-ox-publish-sync ()
;;   "Sync ox-publish exported files to remote server."
;;
;;   ;; Blog source
;;   (magit-status
;;    (concat my-org-publish-directory
;;            "/Blog"))
;;   (magit-stage-modified)
;;   (magit-stage-untracked)
;;   (magit-commit-add-log)
;;   (magit-push-current-to-upstream)
;;
;;   ;; Blog exported files
;;   (magit-status
;;    (concat my-org-publish-directory
;;            "/exported_html/Blog"))
;;   (magit-stage-modified)
;;   (magit-stage-untracked)
;;   (magit-commit-add-log)
;;   (magit-push-current-to-upstream)
;;   )


;;; [ RSS ]

(require 'ox-rss)


;;; [ Beamer ]

(require 'ox-beamer)


;;; [ Mindmap ]

(require 'ox-freemind)


;;; copy formatted text from org-mode to applications.

(defun my-org-formatted-copy ()
  "Export region to HTML, and copy it to the clipboard."
  (interactive)
  (save-window-excursion
    (let* ((buf (org-export-to-buffer 'html "*org-mode formatted copy*" nil nil t t))
           (html (with-current-buffer buf (buffer-string))))
      (with-current-buffer buf
        (shell-command-on-region
         (point-min)
         (point-max)
         "textutil -stdin -format html -convert rtf -stdout | pbcopy"))
      (kill-buffer buf))))


(provide 'init-my-org-export)

;;; init-my-org-export.el ends here
