;;; init-my-org-search.el --- init for Org search.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(setq org-occur-case-fold-search 'smart)

;;; [ Agenda dispatcher ]

;; (org-agenda-dispatch)

;;; [ org-seek ] -- Searching Org-mode files in directory.

(use-package org-seek
  :ensure t
  :config
  (define-key my-org-prefix (kbd "s") 'org-seek-headlines)
  (define-key my-org-prefix (kbd "S") 'org-seek-string)
  (define-key my-org-prefix (kbd "M-s") 'org-seek-regexp)
  )

;;; [ helm-org-rifle ] -- Rifle through your Org buffers and acquire your target.

(use-package helm-org-rifle
  :ensure t
  :config
  (setq helm-org-rifle-show-path t
        helm-org-rifle-fontify-headings t
        helm-org-rifle-show-todo-keywords t
        helm-org-rifle-show-tags t
        helm-org-rifle-directories-recursive t)
  
  (unless (boundp 'org-rifle-prefix)
    (define-prefix-command 'org-rifle-prefix))
  (define-key my-org-prefix (kbd "g") 'org-rifle-prefix)

  (define-key org-rifle-prefix (kbd "g") 'helm-org-rifle-current-buffer)
  (define-key org-rifle-prefix (kbd "G") 'helm-org-rifle)
  (define-key org-rifle-prefix (kbd "d") 'helm-org-rifle-directories)
  (define-key org-rifle-prefix (kbd "f") 'helm-org-rifle-files)
  (define-key org-rifle-prefix (kbd "o") 'helm-org-rifle-org-directory)
  (define-key org-rifle-prefix (kbd "a") 'helm-org-rifle-agenda-files)
  )



(provide 'init-my-org-search)

;;; init-my-org-search.el ends here
