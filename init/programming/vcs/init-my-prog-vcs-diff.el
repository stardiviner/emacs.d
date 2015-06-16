;;; init-my-prog-vcs-diff.el --- init for Diff
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


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


;;; [ diffview ] -- view diff side by side

;;; Usage:
;;
;; - `diffview-current' : Opens the current buffer with diffview.
;; - `diffview-region' : Opens the current region with diffview.

(require 'diffview)



(provide 'init-my-prog-vcs-diff)

;;; init-my-prog-vcs-diff.el ends here
