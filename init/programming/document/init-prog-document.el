;;; init-prog-document.el --- init for Programming Document Look Up.

;;; Commentary:

;;; Code:


(unless (boundp 'document-prefix)
  (define-prefix-command 'document-prefix))
(global-set-key (kbd "C-h d") 'document-prefix)

(require 'init-prog-document-eldoc)
(require 'init-prog-document-man)
;; (require 'init-prog-document-rfc)
(require 'init-prog-document-api)
(require 'init-prog-document-assistant)
(require 'init-prog-document-wikipedia)


(provide 'init-prog-document)

;;; init-prog-document.el ends here
