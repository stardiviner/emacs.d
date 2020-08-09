;;; init-emacs-kill-ring.el --- init for Emacs Kill Ring.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ undo-tree ] -- treat undo history as a tree.

(use-package undo-tree
  :ensure t
  :delight undo-tree-mode
  :custom ((undo-tree-visualizer-diff t)
           (undo-tree-enable-undo-in-region t))
  :init (global-undo-tree-mode 1)
  (add-to-list 'display-buffer-alist
               '("^ \\*undo-tree\\*"
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side . bottom)
                 (slot . 1)
                 (window-height . 0.3)))
  :config
  ;; disable `zoom' before launch `undo-tree'.
  (when (featurep 'zoom)
    (advice-add 'undo-tree-visualize :before 'zoom--off)
    (advice-add 'undo-tree-visualizer-quit :after 'zoom--on)))


(provide 'init-emacs-kill-ring)

;;; init-emacs-kill-ring.el ends here
