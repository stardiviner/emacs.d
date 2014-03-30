;;; init-my-emacs-edit.el --- init Emacs editing
;;; Commentary:

;;; Code:

;;; [ undo-tree ]
;; [C-x u] -> undo-tree-visualizer-mode
;;    `- [C-p/n]  -- move up/down
;;    `- [C-b/f]  -- move left/right
;;    `- t    -- timestamp
;;    `- q    -- quit

(require 'undo-tree)
(global-undo-tree-mode t)
(diminish 'undo-tree-mode)





(provide 'init-my-emacs-edit)

;;; init-my-emacs-edit.el ends here
