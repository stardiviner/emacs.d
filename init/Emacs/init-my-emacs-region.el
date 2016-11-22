;;; init-my-emacs-region.el --- init for Emacs region.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;; typed text replaces the active selection
(delete-selection-mode t)


;;; [ expand-region ]

;;; Expand region increases the selected region by semantic units. Just keep
;;; pressing the key until it selects what you want.

(use-package expand-region
  :ensure t
  :init
  (global-set-key (kbd "C-=") 'er/expand-region)
  )


(provide 'init-my-emacs-region)

;;; init-my-emacs-region.el ends here
