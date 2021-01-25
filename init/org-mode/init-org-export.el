;;; init-org-export.el --- init for Org Exporting
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(use-package ox
  :defer t
  :init
  (setq org-export-with-clocks t
        org-export-with-drawers t
        org-export-with-properties t
        org-export-with-planning t
        ;; don't raise error exception when handling broken link to interrupt exporting process.
        org-export-with-broken-links 'mark)

  ;; Org-mode Babel
  ;; preserve spacing in src blocks when export LaTeX to PDF.
  (setq TeX-auto-untabify t)
  ;; `org-export-use-babel'
  ;; export with results but don't re-evaluate src blocks.
  (setq org-export-use-babel nil)
  ;; (add-to-list 'org-babel-default-header-args '(:exports . "both"))
  (add-to-list 'org-babel-default-header-args '(:eval . "never-export"))
  ;; exclude org headlines exporting with a specific tags.
  (setq org-export-exclude-tags '("noexport")))

;;; [ ox-latex ]

(use-package org-latex-exp-conf
  :defer t
  :commands (org-latex-exp-conf-mode))

;;; [ ox-html ]

(use-package ox-html
  :defer t
  :init (setq org-html-doctype "html5"
              org-html-html5-fancy t
              org-html-use-infojs t ; 'when-configured
              org-html-htmlize-output-type 'inline-css ; make HTML self-containing
              org-html-wrap-src-lines t ; wrap individual lines into <code> element.
              )
  :config (add-to-list 'org-export-global-macros '("kbd" . "@@html:<kbd>$1</kbd>")))

;;; Exporting JavaScript babel code block into <script> tag in HTML export.

;; (add-to-list 'org-src-lang-modes '("inline-js" . javascript))
;; (add-to-list 'org-src-lang-modes '("inline-js" . js))
;; (defvar org-babel-default-header-args:inline-js
;;   '((:results . "html")
;;     (:exports . "results")))
;; (defun org-babel-execute:inline-js (body _params)
;;   (format "<script type=\"text/javascript\">\n%s\n</script>" body))


;;; load `ox-md' for exporting to Markdown.
(with-eval-after-load 'ox
  (require 'ox-md))

;;; [ org-mime ] -- org-mime can be used to send HTML email using Org-mode HTML export.

(use-package org-mime
  :ensure t
  :defer t
  :bind (:map Org-prefix ("M" . org-mime-org-buffer-htmlize)
              :map message-mode-map ("C-c M-o" . org-mime-htmlize))
  :hook ((message-send . org-mime-confirm-when-no-multipart) ; remind you didn't use HTML
         (message-send . org-mime-htmlize)) ; automatically htmlize message on sending email.
  :config
  (add-hook 'org-mime-html-hook
            (lambda ()
              ;; change <pre /> source code block style.
              (org-mime-change-element-style
               "pre" (format "color: %s; background-color: %s; padding: 0.5em;"
                             "#E6E1DC" "#232323"))
              ;; the following can be used to nicely offset block quotes in email bodies.
              (org-mime-change-element-style
               "blockquote" "border-left: 2px solid gray; padding-left: 4px;")))
  
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
      (mapc #'mml-attach-file files))))

;;; [ html2org ] -- convert html to org format text.

(use-package html2org
  :ensure t
  :defer t
  :commands (html2org))

;;; [ ox-epub ] -- Org-mode EPUB export.

(use-package ox-epub
  :ensure t
  :defer t
  :commands (org-epub-export-to-epub))

;;; [ ox-pandoc ] -- another org-mode exporter via pandoc.

(use-package ox-pandoc
  :ensure t
  :defer t)

;;; [ htmlize ] -- deprecated htmlize implementation.

;; (use-package htmlize
;;   :ensure t
;;   :defer t)

;;; [ htmlfontify ] -- try to replace `htmlize'.

(use-package htmlfontify
  :ensure t
  :defer t)


(provide 'init-org-export)

;;; init-org-export.el ends here
