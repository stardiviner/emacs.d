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

;;; [ oddmuse ] --

;; (defun my/oddmuse-setup ()
;;   (require 'company-oddmuse)
;;   (my-company-add-backend-locally 'company-oddmuse))
;; (add-hook 'mediawiki-mode-hook #'my/oddmuse-setup)

;;; [ kiwix ] -- Kiwix client for Emacs.

(use-package kiwix
  :ensure t
  :after org
  :commands (kiwix-launch-server kiwix-at-point-interactive)
  :bind (:map document-prefix ("w" . kiwix-at-point-interactive))
  :custom ((kiwix-server-use-docker t)
           (kiwix-server-port 8089)
           (kiwix-default-library "wikipedia_en_all_2016-02.zim") ; "wikipedia_zh_all_2015-11.zim"
           (kiwix-default-browser-function 'eaf-open-browser))
  :hook (org-load . org-kiwix-setup-link))


(provide 'init-prog-document-wikipedia)

;;; init-prog-document-wikipedia.el ends here
