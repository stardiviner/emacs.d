;;; init-emacs-kill-ring.el --- init for Emacs Kill Ring.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ undo-tree ] -- treat undo history as a tree.

(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode 1)
  (setq undo-tree-visualizer-diff t
        undo-tree-visualizer-relative-timestamps t)
  (add-to-list 'display-buffer-alist
               '("^ \\*undo-tree\\*"
                 display-buffer-reuse-window display-buffer-in-side-window
                 (reusable-frames . visible)
                 (side . right)
                 (slot . 1)
                 (window-width . 0.5))))


(provide 'init-emacs-kill-ring)

;;; init-emacs-kill-ring.el ends here
