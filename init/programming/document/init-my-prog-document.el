;;; init-my-prog-document.el --- init for Programming Document Look Up.

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

(require 'init-my-prog-document-eldoc)
(require 'init-my-prog-document-info)
(require 'init-my-prog-document-man)
;; (require 'init-my-prog-document-rfc)
(require 'init-my-prog-document-api)
(require 'init-my-prog-document-assistant)
(require 'init-my-prog-document-wikipedia)


(provide 'init-my-prog-document)

;;; init-my-prog-document.el ends here
