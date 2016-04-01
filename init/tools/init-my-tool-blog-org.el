;;; init-my-tool-blog-org.el --- init for Org-mode
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ ox-publish ]

(require 'ox-publish)

;; (setq org-publish-project-alist
;;       '(("Blog"
;;          :base-directory "~/Org/Diary/Public"
;;          :recursive t
;;          )
;;         ("Wiki"
;;          :base-directory "~/Org/Wiki"
;;          :base-extension 'any
;;          ;; :exclude
;;          ;; :include
;;          :recursive t
;;          )
;;         ;; ("Gallery"
;;         ;;  ;; :base-directory "~/Org/"
;;         ;;  :recursive t
;;         ;;  )
;;         ("Website"
;;          :components ("Blog" "Gallery")
;;          ;; can I set those variable at here for all of above publishings ?
;;          :publishing-directory "~/WWW/Org-publish"
;;          :publishing-function org-html-publish-to-html
;;          ;; :preparation-function
;;          ;; :completion-function
;;          :htmlized-source t
;;          ;; :headline-levels 4
;;          :section-number t
;;          ;; :language
;;          ;;; [ author ]
;;          :with-author "stardiviner"
;;          :with-email "numbchild@gmail.com"
;;          :with-footnotes "Get over the world!"
;;          ;; :with-latex
;;          :with-sub-superscript t
;;          :with-tables t
;;          :with-tags t
;;          ;;; [ tasks ]
;;          ;; :with-tasks t
;;          ;; :with-planning
;;          :with-todo-keywords
;;          ;; :with-priority
;;          :with-timestamps t
;;          ;; :with-toc
;;          ;;; [ html ]
;;          ;; :html-doctype "html5"
;;          ;; :html-xml-declaration
;;          ;; :html-link-up
;;          ;; :html-link-home
;;          ;; :html-head
;;          ;; :html-head-extra
;;          ;; :html-head-include-default-style
;;          ;; :html-head-include-scripts
;;          ;; :html-inline-images t
;;          ;; :html-preamble
;;          ;; :html-postamble
;;          ;; :html-table-attributes
;;          ;;; [ sitemap ]
;;          :auto-sitemap t
;;          ;; When non-nil, remove filenames' extensions from the generated sitemap. Useful to have cool URIs.
;;          :sitemap-sans-extension t
;;          ;;; [ index ]
;;          ;; When non-nil, generate in index in the file `theindex.org' and publish it as `theindex.html'.
;;          ;;
;;          ;; The file will be created when first publishing a project with the
;;          ;; :makeindex set. The file only contains a statement #+INCLUDE:
;;          ;; "theindex.inc". You can then build around this include statement by
;;          ;; adding a title, style information, etc.
;;          :makeindex t
;;          )
;;         )
;;       )

;; (setq org-publish-use-timestamps-flag nil)

;; Publishing to a local directory is also much faster than to a remote one, so
;; that you can afford more easily to republish entire projects. If you set
;; `org-publish-use-timestamps-flag' to nil, you gain the main benefit of
;; re-including any changed external files such as source example files you
;; might include with #+INCLUDE:. The timestamp mechanism in Org is not smart
;; enough to detect if included files have been modified.


;;; [ ox-rss ] -- RSS 2.0 Back-End for Org Export Engine.

;; (require 'ox-rss)


(provide 'init-my-tool-blog-org)

;;; init-my-tool-blog-org.el ends here
