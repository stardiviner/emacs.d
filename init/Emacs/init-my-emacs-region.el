;;; init-my-emacs-region.el --- init for Emacs region.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ expand-region ]

;;; Expand region increases the selected region by semantic units. Just keep
;;; pressing the key until it selects what you want.

(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region)  
  )


;;; [ awk-it ] -- run AWK interactively on region!

;;; Usage:
;;
;; - `awk-it-' prefix
;; - [M-x awk-it]

(use-package awk-it
  :config
  (define-key my-search-prefix (kbd "w") 'awk-it)
  )


(provide 'init-my-emacs-region)

;;; init-my-emacs-region.el ends here
