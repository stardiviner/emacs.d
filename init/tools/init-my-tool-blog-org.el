;;; init-my-tool-blog-org.el --- init for Org-mode
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;;_* Publishing

;; - [C-c C-e P x] -- (org-publish)
;; - [C-c C-e P p] -- (org-publish-current-project)
;; - [C-c C-e P f] -- (org-publish-current-file)
;; - [C-c C-e P a] -- (org-publish-all)

(require 'ox-publish)

;; Each element of the list configures one project, and may be in one of the two following forms:
;; In both cases, projects are configured by specifying property values. A
;; project defines the set of files that will be published, as well as the
;; publishing configuration to use when publishing those files. When a project
;; takes the second form listed above, the individual members of the :components
;; property are taken to be sub-projects, which group together files requiring
;; different publishing options. When you publish such a “meta-project”, all the
;; components will also be published, in the sequence given.

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
;;         ;;  ;; TODO :base-directory "~/Org/"
;;         ;;  :recursive t
;;         ;;  )
;;         ("Website"
;;          :components ("Blog" "Gallery")
;;          ;; TODO can I set those variable at here for all of above publishings ?
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
;;          ;; TODO :html-doctype "html5"
;;          ;; TODO :html-xml-declaration
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


;;; [ Blog ]


;;; [ o-blog ] -- A stand-alone blog and publication tool for Org-mode.

;;; Usage:
;;
;; Publish:
;;   - Open the ~/.emacs.d/o-blog/example/sample.org file
;;   - type M-x org-publish-blog
;;   - The result site would be published by default in ~/.emacs.d/o-blog/out.

;; (require 'o-blog)



;; (require 'org-blog)


;;; [ org2jekyll ]


;;; [ org-jekyll ]

;; (require 'org-jekyll)




(provide 'init-my-tool-blog-org)

;;; init-my-tool-blog-org.el ends here
