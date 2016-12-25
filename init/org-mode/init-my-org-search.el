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


(provide 'init-my-org-search)

;;; init-my-org-search.el ends here
