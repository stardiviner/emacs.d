;;; init-emacs-clipboard.el --- init for Emacs clipboard
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ clipboard ]

;; - select-enable-primary - default nil; set this to t if you want the Emacs commands C-w and C-y to use the primary selection.
;; - select-enable-clipboard - default t; set this to nil if you want the Emacs commands C-w and C-y to use the clipboard selection.
;; - Yes, you can have Emacs use both at the same time.
;; - `clipboard-yank'
;; - `clipboard-kill-ring-save'

(setq select-enable-clipboard t
      select-enable-primary t
      x-select-enable-clipboard-manager t)

;; (set-clipboard-coding-system 'utf-8)

;;; [ xclip ] --- use xclip to copy&paste from the X clipboard when running in xterm.

;; (use-package xclip
;;   :ensure t
;;   :init (xclip-mode 1))

(provide 'init-emacs-clipboard)

;;; init-emacs-clipboard.el ends here
