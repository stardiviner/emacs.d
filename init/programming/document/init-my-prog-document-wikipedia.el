;;; init-my-prog-document-wikipedia.el --- init for Offline Wikipedia
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ kiwix ] -- Kiwix client for Emacs.

(use-package kiwix
  ;; :ensure t
  :config
  (define-key my-prog-help-document-map (kbd "w") 'kiwix-serve-query)
  (define-key my-prog-help-document-map (kbd "W") 'kiwix-serve-index)
  )


(provide 'init-my-prog-document-wikipedia)

;;; init-my-prog-document-wikipedia.el ends here
