;;; init-my-emacs-kill-ring.el --- init for Emacs Kill Ring.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; undo-tree

;;; Usage:
;;
;; [C-x u] -> undo-tree-visualizer-mode
;;    `- [C-p/n]  -- move up/down
;;    `- [C-b/f]  -- move left/right
;;    `- t    -- timestamp
;;    `- q    -- quit

(use-package undo-tree
  :config
  (setq undo-tree-visualizer-diff t
        undo-tree-visualizer-relative-timestamps t)
  )


;;; kill-ring-search

;; Copied something important half an hour ago? Tired of hitting M-y 20 times?
;; Now you can search the kill ring incrementally and yank the result!
;; Just hit M-C-y to start the search. M-y and C-y work as usual.

;; (require 'kill-ring-search)
;;
;; (autoload 'kill-ring-search "kill-ring-search"
;;   "Search the kill ring in the minibuffer."
;;   (interactive))
;;
;; (global-set-key (kbd "C-M-y") 'kill-ring-search)


;;; kill-ring-ido

;;; Usage:
;; - [C-M-y] ::

;; (if (featurep 'ido)
;;     (require 'kill-ring-ido)
;;   (global-set-key (kbd "C-M-y") 'kill-ring-ido))


(provide 'init-my-emacs-kill-ring)

;;; init-my-emacs-kill-ring.el ends here
