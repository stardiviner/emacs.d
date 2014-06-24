;;; init-my-emacs-sidebar.el --- init sidebar utilities
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ speedbar ]



;;; [ sr-speedbar ]



;;; [ emacs-neotree ]

;;; Usage

;; [F8] -- toggle.

;; Buffer shortcuts

;;     n - next line
;;     p - previous line
;;     SPC or RET or TAB - open file / toggle expand folder
;;     g - refresh tree
;;     A - stretch neotree window
;;     C-c C-n - create file or directory
;;     C-c C-d - delete file or directory
;;     C-c C-c - change root directory

;; Commands

;;     M-x neotree-dir RET
;;     M-x neotree-show RET or M-x neotree RET
;;     M-x neotree-hide RET
;;     M-x neotree-toggle


(require 'neotree)

(global-set-key [f8] 'neotree-toggle)

(setq neo-persist-show nil
      neo-window-width 25
      neo-show-header t)


(provide 'init-my-emacs-sidebar)

;;; init-my-emacs-sidebar.el ends here
