;;; init-my-prog-lang-html.el --- init HTML for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ sgml-mode ] -- major mode for editing SGML documents.

;; Makes > match <.
;; Keys <, &, SPC within <>, ", / and ' can be electric depending on
;; `sgml-quick-keys'.


;;; [ ac-html ] -- Provide accurate and intelligent auto completion to HTML and CSS.

;; ;;; If you are using html-mode:
;; (add-hook 'html-mode-hook 'ac-html-enable)
;; ;;; If you are using web-mode:
;; ;;; Additionally you need to add these lines:
;; (unless (featurep 'web-mode)
;;   (require 'web-mode))
;; (add-to-list 'web-mode-ac-sources-alist
;;              '("html" . (ac-source-html-attribute-value
;;                          ac-source-html-tag
;;                          ac-source-html-attribute)))
;;
;; ;;; Support for template languages:
;; (add-hook 'haml-mode-hook 'ac-haml-enable)
;; (add-hook 'jade-mode-hook 'ac-jade-enable)
;; (add-hook 'slim-mode-hook 'ac-slim-enable)


(add-hook 'web-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends)
                         'company-nxml)
            (add-to-list (make-local-variable 'company-backends)
                         'company-web-html)
            (add-to-list (make-local-variable 'company-backends)
                         'company-css)
            ))



(require 'init-my-prog-lang-html5)


(provide 'init-my-prog-lang-html)

;;; init-my-prog-lang-html.el ends here
