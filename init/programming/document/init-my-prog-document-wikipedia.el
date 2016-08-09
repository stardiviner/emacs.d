;;; init-my-prog-document-wikipedia.el --- init for Offline Wikipedia
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ mediawiki ] -- mediawiki frontend.

(use-package mediawiki
  :ensure t)


;;; [ ox-mediawiki ] -- Mediawiki Back-End for Org Export Engine.

;; Open a .org document and run `org-mw-export-as-mediawiki'.

(use-package ox-mediawiki
  :ensure t)


;;; [ kiwix ] -- Kiwix client for Emacs.

(use-package kiwix
  :ensure t
  :config
  (define-key my-prog-help-document-map (kbd "w") 'kiwix-at-point)
  (define-key my-prog-help-document-map (kbd "W") 'kiwix-at-point-interactive)
  (define-key my-prog-help-document-map (kbd "C-w") 'kiwix-launch-server)
  )


(provide 'init-my-prog-document-wikipedia)

;;; init-my-prog-document-wikipedia.el ends here
