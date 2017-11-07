;;; init-my-prog-vcs-diff.el --- init for Diff
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(unless (boundp 'prog-vcs-diff-prefix)
  (define-prefix-command 'prog-vcs-diff-prefix))
(define-key prog-vcs-prefix (kbd "d") 'prog-vcs-diff-prefix)

;;; [ diff ]

(use-package diff
  :config
  (set-face-attribute 'diff-file-header nil
                      :foreground "orange"
                      :weight 'bold)
  (set-face-attribute 'diff-hunk-header nil
                      :foreground "sky blue")
  (set-face-attribute 'diff-context nil
                      :foreground "dark gray")
  (set-face-attribute 'diff-removed nil
                      :weight 'normal
                      :background (color-darken-name (face-background 'default) 5)
                      :foreground "red3")
  (set-face-attribute 'diff-added nil
                      :weight 'normal
                      :background (color-darken-name (face-background 'default) 5)
                      :foreground "forest green")
  (set-face-attribute 'diff-refine-removed nil
                      :inherit 'diff-removed
                      :inverse-video nil :weight 'bold
                      :background "black" :foreground "red")
  (set-face-attribute 'diff-refine-added nil
                      :inherit 'diff-added
                      :inverse-video nil :weight 'bold
                      :background "black" :foreground "green")
  )

;;; [ ediff ]

(use-package ediff
  :config
  (setq ediff-use-faces t)

  ;; Even numbered
  (set-face-attribute 'ediff-even-diff-Ancestor nil
                      :background "#222222")
  (set-face-attribute 'ediff-even-diff-A nil
                      :background (color-darken-name (face-background 'default) 7))
  (set-face-attribute 'ediff-even-diff-B nil
                      :background (color-darken-name (face-background 'default) 7))
  (set-face-attribute 'ediff-even-diff-C nil
                      :background (color-darken-name (face-background 'default) 7))

  ;; Odd numbered
  (set-face-attribute 'ediff-odd-diff-Ancestor nil
                      :background "#444444")
  (set-face-attribute 'ediff-odd-diff-A nil
                      :background (color-darken-name (face-background 'default) 3))
  (set-face-attribute 'ediff-odd-diff-B nil
                      :background (color-darken-name (face-background 'default) 3))
  (set-face-attribute 'ediff-odd-diff-C nil
                      :background (color-darken-name (face-background 'default) 3))


  ;; change default ediff style
  ;; don't start another frame
  (setq ediff-window-setup-function 'ediff-setup-windows-plain) ; 'ediff-setup-windows-default
  ;; put windows side by side
  (setq ediff-split-window-function 'split-window-horizontally)
  ;; revert windows on exit (needs winner mode)
  (winner-mode 1)
  ;; (add-hook 'ediff-before-setup-windows-hook #'winner-mode)
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo)
  )


;;; [ diffview ] -- render a unified diff to side-by-side format.

(use-package diffview
  :ensure t
  :defer t
  :bind (:map prog-vcs-diff-prefix
              ("d" . diffview-current)
              ("r" . diffview-region)
              ("m" . diffview-message))
  )


;;; [ smerge-mode ] -- simplify editing output from the diff3 program.

(use-package smerge-mode
  :ensure t
  :bind (:map smerge-mode-map
              ("M-g n" . smerge-next)
              ("M-g p" . smerge-prev)
              ("M-g k c" . smerge-keep-current)
              ("M-g k m" . smerge-keep-mine)
              ("M-g k o" . smerge-keep-other)
              ("M-g k b" . smerge-keep-base)
              ("M-g k a" . smerge-keep-all)
              ("M-g e" . smerge-ediff)
              ("M-g K" . smerge-kill-current)
              ("M-g m" . smerge-context-menu)
              ("M-g M" . smerge-popup-context-menu)
              )
  :config
  ;;; keybindings
  ;; (setq smerge-command-prefix (kbd "C-c v M-d"))

  ;; enable `smerge-mode' automatically
  (defun smart-try-smerge ()
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
        (smerge-mode 1))))

  (add-hook 'find-file-hook 'smart-try-smerge t)
  (add-hook 'after-revert-hook 'smerge-try-smerge t)
  )

;;; [ dumb-diff ] -- An Emacs package to fast arbitrary diffs.

(use-package dumb-diff
  :ensure t
  :defer t
  :commands (dumb-diff)
  )


(provide 'init-my-prog-vcs-diff)

;;; init-my-prog-vcs-diff.el ends here
