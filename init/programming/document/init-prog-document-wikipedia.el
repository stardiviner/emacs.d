;;; init-prog-document-wikipedia.el --- init for Offline Wikipedia
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ mediawiki ] -- mediawiki frontend.

;; (use-package mediawiki
;;   :ensure t
;;   :defer t)


;;; [ ox-mediawiki ] -- Mediawiki Back-End for Org Export Engine.

;; Open a .org document and run `org-mw-export-as-mediawiki'.

;; (use-package ox-mediawiki
;;   :ensure t
;;   :defer t)


;;; [ kiwix ] -- Kiwix client for Emacs.

;; (use-package kiwix
;;   :ensure t
;;   :after org
;;   :functions (org-link-set-parameters)
;;   :defer t
;;   :bind (:map prog-doc-map
;;               ("w" . kiwix-at-point)
;;               ("W" . kiwix-at-point-interactive)
;;               ("M-w" . kiwix-launch-server))
;;   :init
;;   (setq kiwix-your-language-library "zh"))


(provide 'init-prog-document-wikipedia)

;;; init-prog-document-wikipedia.el ends here
