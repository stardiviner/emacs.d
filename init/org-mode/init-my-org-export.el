;;; init-my-org-export.el --- init for Org Exporting
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(require 'ox)

(setq org-export-headline-levels 4
      org-export-with-clocks t
      org-export-with-planning t
      org-export-with-timestamps t
      org-export-with-properties t
      ;; org-export-use-babel nil
      )

;;; exclude org headlines exporting with a specific tags.
(setq org-export-exclude-tags '("noexport"))

;;; ------------- Export UTF-8 checkboxes ---------------------
;;; This snippet turns - [X] into ☑ and - [ ] into ☐.

;; 'ascii, 'unicode, 'html
(setq org-html-checkbox-type 'unicode)


;;; ox-html
(require 'ox-html)

(setq org-html-doctype "html5"
      org-html-html5-fancy t
      org-html-use-infojs t ; 'when-configured
      ;; org-html-infojs-options
      ;; org-html-allow-name-attribute-in-anchors t
      org-html-htmlize-output-type 'inline-css ; make HTML self-containing

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
      
      ;; org-html-link-home ""
      ;; org-html-link-up ""
      ;; org-html-link-use-abs-url t ; relative to HTML_LINK_HOME

      ;; org-html-head ""
      ;; org-html-head-extra ""

      ;; org-html-viewport
      )

;;; Exporting JavaScript babel code block into <script> tag in HTML export.

;; (add-to-list 'org-src-lang-modes '("inline-js" . javascript))
(add-to-list 'org-src-lang-modes '("inline-js" . js2))
(defvar org-babel-default-header-args:inline-js
  '((:results . "html")
    (:exports . "results")))
(defun org-babel-execute:inline-js (body _params)
  (format "<script type=\"text/javascript\">\n%s\n</script>" body))


;;; convert selected region to Markdown and copy to clipboard for pasting
;;; on sites like GitHub, and Stack Overflow.

(defun my-org-md-convert-region-to-md ()
  "Convert selected region to Markdown and copy to clipboard.

For pasting on sites like GitHub, and Stack Overflow."
  (interactive)
  (require 'ox-md)
  
  ;; (with-temp-buffer-window
  ;;  "*org->markdown temp*"
  ;;  (org-export-to-buffer 'md "*org->markdown temp*"
  ;;    async subtreep visible-only) 'delete-window)

  (unless (org-region-active-p) (user-error "No active region to replace"))
  (x-set-selection 'CLIPBOARD
                   (org-export-string-as
                    (buffer-substring (region-beginning) (region-end)) 'md t))
  (deactivate-mark))

(unless (boundp 'paste-prefix)
  (define-prefix-command 'paste-prefix))
(define-key paste-prefix (kbd "m") 'my-org-md-convert-region-to-md)


;;; [ org-mime ] -- org-mime can be used to send HTML email using Org-mode HTML export.

(use-package org-mime
  :ensure t
  :bind (:map Org-prefix
              ("m" . org-mime-org-buffer-htmlize)
              :map message-mode-map
              ("C-c M-o" . org-mime-htmlize)
              )
  :config
  (add-hook 'org-mime-html-hook
            (lambda ()
              ;; change <pre /> source code block style.
              (org-mime-change-element-style
               "pre" (format "color: %s; background-color: %s; padding: 0.5em;"
                             "#E6E1DC" "#232323"))
              ;; the following can be used to nicely offset block quotes in email bodies.
              (org-mime-change-element-style
               "blockquote" "border-left: 2px solid gray; padding-left: 4px;")
              ))
  
  ;; Out of the box, =org-mime= does not seem to attach file links to emails or
  ;; make images for equations.
  ;; Here is an adaptation of =org-mime-compose= that does that for html messages.

  (defun org-mime-compose (body fmt file &optional to subject headers)
    "Make `org-mime-compose' support attach file for HTML messages."
    (require 'message)
    (let ((bhook
           (lambda (body fmt)
             (let ((hook (intern (concat "org-mime-pre-"
                                         (symbol-name fmt)
                                         "-hook"))))
               (if (> (eval `(length ,hook)) 0)
                   (with-temp-buffer
                     (insert body)
                     (goto-char (point-min))
                     (eval `(run-hooks ',hook))
                     (buffer-string))
                 body))))
          (fmt (if (symbolp fmt) fmt (intern fmt)))
          (files (org-element-map (org-element-parse-buffer) 'link
                   (lambda (link)
                     (when (string= (org-element-property :type link) "file")
                       (file-truename (org-element-property :path link)))))))
      (compose-mail to subject headers nil)
      (message-goto-body)
      (cond
       ((eq fmt 'org)
        (require 'ox-org)
        (insert (org-export-string-as
                 (org-babel-trim (funcall bhook body 'org)) 'org t)))
       ((eq fmt 'ascii)
        (require 'ox-ascii)
        (insert (org-export-string-as
                 (concat "#+Title:\n" (funcall bhook body 'ascii)) 'ascii t)))
       ((or (eq fmt 'html) (eq fmt 'html-ascii))
        (require 'ox-ascii)
        (require 'ox-org)
        (let* ((org-link-file-path-type 'absolute)
               ;; we probably don't want to export a huge style file
               (org-export-htmlize-output-type 'inline-css)
               (org-html-with-latex 'dvipng)
               (html-and-images
                (org-mime-replace-images
                 (org-export-string-as (funcall bhook body 'html) 'html t)))
               (images (cdr html-and-images))
               (html (org-mime-apply-html-hook (car html-and-images))))
          (insert (org-mime-multipart
                   (org-export-string-as
                    (org-babel-trim
                     (funcall bhook body (if (eq fmt 'html) 'org 'ascii)))
                    (if (eq fmt 'html) 'org 'ascii) t)
                   html)
                  (mapconcat 'identity images "\n")))))
      (mapc #'mml-attach-file files)))
  )

;;; [ Beamer ]

;; (require 'ox-beamer)

;;; [ org-preview-html ] -- automatically use eww to preview the current org file on save.

;; (use-package org-preview-html
;;   :ensure t
;;   :defer t)

;;; [ org-eww ] -- automatically use eww to preview current org-file when save.

;; (use-package org-eww
;;   :ensure t
;;   :defer t
;;   :init
;;   (add-hook 'org-mode-hook 'org-eww-mode)
;;   )

;;; [ html2org ] -- convert html to org format text.

(use-package html2org
  :ensure t
  :commands (html2org))

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


;;; [ ox-epub ] -- Org-mode EPUB export.

(use-package ox-epub
  :ensure t)

;;; [ ox-pandoc ] -- another org-mode exporter via pandoc.

;; (use-package ox-pandoc
;;   :ensure t)


(provide 'init-my-org-export)

;;; init-my-org-export.el ends here
