;;; init-my-prog-vcs.el --- init Version Control System for Programming
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ vc-mode ] ---

;;; Usage:
;; - [C-x v] -- prefix. `vc-*'.
;; - [C-x v C-h] -- get keybinding with its prefix.



;;; [ diff ]

(set-face-attribute 'diff-file-header nil
                    :foreground "orange"
                    :weight 'bold
                    )
(set-face-attribute 'diff-hunk-header nil
                    :foreground "sky blue"
                    )
(set-face-attribute 'diff-context nil
                    :foreground "dark gray"
                    )
(set-face-attribute 'diff-removed nil
                    :background (color-darken-name (face-background 'default) 5)
                    :foreground "red3"
                    :weight 'normal)
(set-face-attribute 'diff-added nil
                    :background (color-darken-name (face-background 'default) 5)
                    :foreground "forest green"
                    :weight 'normal)
(set-face-attribute 'diff-refine-removed nil
                    :inherit 'diff-removed
                    :background "black"
                    :foreground "red"
                    :inverse-video nil
                    :weight 'bold
                    )
(set-face-attribute 'diff-refine-added nil
                    :inherit 'diff-added
                    :background "black"
                    :foreground "green"
                    :inverse-video nil
                    :weight 'bold
                    )



(require 'init-my-prog-vcs-git)
(require 'init-my-prog-vcs-diff)


(provide 'init-my-prog-vcs)

;;; init-my-prog-vcs.el ends here
