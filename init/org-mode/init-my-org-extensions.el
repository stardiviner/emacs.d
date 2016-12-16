;;; init-my-org-extensions.el --- init for Org Extensions
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ org-plot ] -- Support for plotting from Org-mode.

(require 'org-plot)


;;; [ helm-org-rifle ] -- Rifle through your Org buffers and acquire your target.

(use-package helm-org-rifle
  :ensure t
  :defer t
  :init
  (define-key my-org-prefix (kbd "g") 'helm-org-rifle-current-buffer)
  (define-key my-org-prefix (kbd "G") 'helm-org-rifle)
  :config
  (setq helm-org-rifle-show-path t
        helm-org-rifle-fontify-headings t
        helm-org-rifle-show-todo-keywords t
        helm-org-rifle-show-tags t)
  )


;;; [ otama ] -- Simple org-table based database, intended to be a light version of BBDB and helm-friendly.

;; (use-package otama
;;   :ensure t
;;   :defer t
;;   :init
;;   (setq otama-database-file-name
;;         (concat (getenv "HOME") "/Org" "/otama/otama.org"))
;;
;;   (define-key my-org-prefix (kbd "D") 'otama-helm)
;;   )


;;; [ org-eww ] -- automatically use eww to preview current org-file when save.

;; (use-package org-eww
;;   :ensure t
;;   :defer t
;;   :init
;;   (add-hook 'org-mode-hook 'org-eww-mode)
;;   )


(provide 'init-my-org-extensions)

;;; init-my-org-extensions.el ends here
