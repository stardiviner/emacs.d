;;; init-my-emacs-kill-ring.el --- init for Emacs Kill Ring.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(setq save-interprogram-paste-before-kill t)

;;; [ undo-tree ]

(use-package undo-tree
  :ensure t
  :defer t
  :config
  (setq undo-tree-visualizer-diff t
        undo-tree-visualizer-relative-timestamps t)

  (global-undo-tree-mode 1)
  )


(provide 'init-my-emacs-kill-ring)

;;; init-my-emacs-kill-ring.el ends here
