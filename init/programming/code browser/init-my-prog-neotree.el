;;; init-my-prog-neotree.el --- 
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

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

(setq neo-persist-show nil
      neo-window-width 25
      neo-show-header t)

(add-hook 'after-init-hook 'neotree-show)
(global-set-key [f6] 'neotree-toggle) ; open or switch to project-explorer sidebar.



(provide 'init-my-prog-neotree)

;;; init-my-prog-neotree.el ends here
