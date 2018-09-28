;;; init-emacs-region.el --- init for Emacs region.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Selection ]

;;; transient-mark-mode
;;
;; NOTE: it is very heavy for large region operation like deletion.
;; It has very frequent property-notify events.
;; (transient-mark-mode t)


;; typed text replaces the active selection
;; (delete-selection-mode t)


;;; [ expand-region ]

;;; Expand region increases the selected region by semantic units. Just keep
;;; pressing the key until it selects what you want.

(use-package expand-region
  :ensure t
  :defer t
  :bind ("C-=" . er/expand-region))


(provide 'init-emacs-region)

;;; init-emacs-region.el ends here
