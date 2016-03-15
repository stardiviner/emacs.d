;;; init-my-prog-document.el --- init for Programming Document Look Up.

;;; Commentary:

;;; Code:


(unless (boundp 'my-prog-help-document-map)
  (define-prefix-command 'my-prog-help-document-map))
(global-set-key (kbd "C-h d") 'my-prog-help-document-map)

;; example
;; (add-hook 'lisp-mode-hook
;;           '(lambda ()
;;              (local-set-key (kbd "C-h d d") 'sly-documentation-lookup)
;;              ))

(require 'init-my-prog-document-info)
(require 'init-my-prog-document-man)
;; (require 'init-my-prog-document-rfc)
(require 'init-my-prog-document-api)
(require 'init-my-prog-document-assistant)
(require 'init-my-prog-document-wikipedia)



(provide 'init-my-prog-document)

;;; init-my-prog-document.el ends here
