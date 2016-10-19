;;; init-my-emacs-region.el --- init for Emacs region.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ expand-region ]

;;; Expand region increases the selected region by semantic units. Just keep
;;; pressing the key until it selects what you want.

(use-package expand-region
  :ensure t
  :defer t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region)
  )


(provide 'init-my-emacs-region)

;;; init-my-emacs-region.el ends here
