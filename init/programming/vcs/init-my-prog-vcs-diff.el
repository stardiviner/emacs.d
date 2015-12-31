;;; init-my-prog-vcs-diff.el --- init for Diff
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ ediff ]

;;; Usage:
;;
;; 1. ediff-files
;; 2. |
;; 3. n/p, a/b.
;; 4. q

(require 'ediff)

(setq ediff-use-faces t)

;;; Even numbered
(set-face-attribute 'ediff-even-diff-Ancestor nil
                    :background "#222222")
(set-face-attribute 'ediff-even-diff-A nil
                    :background (color-darken-name (face-background 'default) 7))
(set-face-attribute 'ediff-even-diff-B nil
                    :background (color-darken-name (face-background 'default) 7))
(set-face-attribute 'ediff-even-diff-C nil
                    :background (color-darken-name (face-background 'default) 7))

;;; Odd numbered
(set-face-attribute 'ediff-odd-diff-Ancestor nil
                    :background "#444444")
(set-face-attribute 'ediff-odd-diff-A nil
                    :background (color-darken-name (face-background 'default) 3))
(set-face-attribute 'ediff-odd-diff-B nil
                    :background (color-darken-name (face-background 'default) 3))
(set-face-attribute 'ediff-odd-diff-C nil
                    :background (color-darken-name (face-background 'default) 3))



;;; change default ediff style
;; don't start another frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain) ; 'ediff-setup-windows-default
;; put windows side by side
(setq ediff-split-window-function 'split-window-horizontally)
;; revert windows on exit (needs winner mode)
(winner-mode 1)
;; (add-hook 'ediff-before-setup-windows-hook #'winner-mode)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)


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


;;; [ diffview ] -- render a unified diff to side-by-side format.

;;; Usage:
;;
;; - `diffview-current' :: Opens the current buffer with diffview.
;; - `diffview-region' :: Opens the current region with diffview.
;; - `diffview-message' :: View the current email message (which presumably contains a patch) side-by-side.

(define-key my-prog-vcs-diff-prefix (kbd "d") 'diffview-current)
(define-key my-prog-vcs-diff-prefix (kbd "r") 'diffview-region)
(define-key my-prog-vcs-diff-prefix (kbd "m") 'diffview-message)


;;; [ smerge-mode ]

(use-package smerge-mode
  :config
  ;; (setq smerge-command-prefix (kbd "C-c v d"))

  (define-key smerge-mode-map (kbd "M-g n") 'smerge-next)
  (define-key smerge-mode-map (kbd "M-g p") 'smerge-prev)
  (define-key smerge-mode-map (kbd "M-g k c") 'smerge-keep-current)
  (define-key smerge-mode-map (kbd "M-g k m") 'smerge-keep-mine)
  (define-key smerge-mode-map (kbd "M-g k o") 'smerge-keep-other)
  (define-key smerge-mode-map (kbd "M-g k b") 'smerge-keep-base)
  (define-key smerge-mode-map (kbd "M-g k a") 'smerge-keep-all)
  (define-key smerge-mode-map (kbd "M-g e") 'smerge-ediff)
  (define-key smerge-mode-map (kbd "M-g K") 'smerge-kill-current)
  (define-key smerge-mode-map (kbd "M-g m") 'smerge-context-menu)
  (define-key smerge-mode-map (kbd "M-g M") 'smerge-popup-context-menu)
  )


(provide 'init-my-prog-vcs-diff)

;;; init-my-prog-vcs-diff.el ends here
