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


;; FIXME:
;;; Export Org to latex PDF with color for source code block highlighting.
;; (require 'org-latex)
;; (require 'org-latex-code)
;; (require 'org-latex-color)
;;
;; (setq org-export-latex-packages-alist '("" "listings"))
;; (setq org-export-latex-packages-alist '("" "color"))
;; (setq org-export-latex-listings t)


;;; PDF exporting
;;
;; (setq org-latex-to-pdf-process 
;;       '("pdflatex %f" "biber %b" "pdflatex %f" "pdflatex %f"))


;;; ------------- Export UTF-8 checkboxes ---------------------
;;; This snippet turns - [X] into ☑ and - [ ] into ☐.

;; 'ascii, 'unicode, 'html
(setq org-html-checkbox-type 'ascii)

;; TODO: if upper works, then remove this code.
;; (defun sacha/org-html-checkbox (checkbox)
;;   "Format CHECKBOX into HTML."
;;   (case checkbox (on "<span class=\"check\">&#x2611;</span>") ; checkbox (checked)
;;         (off "<span class=\"checkbox\">&#x2610;</span>")
;;         (trans "<code>[-]</code>")
;;         (t "")))
;; (defadvice org-html-checkbox (around sacha activate)
;;   (setq ad-return-value (sacha/org-html-checkbox (ad-get-arg 0))))


;;; ox-html

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

;;; htmlize
;;
;; (setq org-html-htmlize-font-prefix "org-"
;;       org-html-htmlize-output-type 'inline-css)


;;; convert selected region to Markdown and copy to clipboard for pasting
;;; on sites like GitHub, and Stack Overflow.

(define-key paste-map (kbd "m") 'my-org-md-convert-region-to-md)

(defun my-org-md-convert-region-to-md ()
  "convert selected region to Markdown and copy to clipboard for
pasting on sites like GitHub, and Stack Overflow."
  (interactive)
  (unless (org-region-active-p) (user-error "No active region to replace"))
  (x-set-selection 'CLIPBOARD
                   (org-export-string-as
                    (buffer-substring (region-beginning) (region-end)) 'md t)))


(provide 'init-my-org-export)

;;; init-my-org-export.el ends here
