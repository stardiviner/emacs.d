;;; init-prog-document.el --- init for Programming Document Look Up.

;;; Commentary:

;;; Code:


(unless (boundp 'prog-doc-map)
  (define-prefix-command 'prog-doc-map))
(global-set-key (kbd "C-h d") 'prog-doc-map)

;; example
;; (add-hook 'lisp-mode-hook
;;           '(lambda ()
;;              (local-set-key (kbd "C-h d d") 'sly-documentation-lookup)
;;              ))

(require 'init-prog-document-eldoc)
(require 'init-prog-document-man)
;; (require 'init-prog-document-rfc)
(require 'init-prog-document-api)
(require 'init-prog-document-assistant)
(require 'init-prog-document-wikipedia)


(provide 'init-prog-document)

;;; init-prog-document.el ends here
