;;; init-prog-vcs-diff.el --- init for Diff
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(unless (boundp 'prog-vcs-diff-prefix)
  (define-prefix-command 'prog-vcs-diff-prefix))
(define-key prog-vcs-prefix (kbd "d") 'prog-vcs-diff-prefix)

;;; [ diff ]

;;; [ ediff ]

(use-package ediff
  :ensure t
  :defer t
  :init (setq ediff-use-faces t)
  :config
  ;; change default ediff style: don't start another frame
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
              ("m" . diffview-message)))


;;; [ smerge-mode ] -- simplify editing output from the diff3 program.

(use-package smerge-mode
  :ensure t
  :defer t
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
  ;; (defun smerge-smart-dwim ()
  ;;   "Enable `smerge-mode' automatically."
  ;;   (save-excursion
  ;;     (goto-char (point-min))
  ;;     (when (re-search-forward "^<<<<<<< " nil t)
  ;;       (smerge-mode 1))))
  ;;
  ;; (add-hook 'find-file-hook #'smerge-smart-dwim t)
  ;; (add-hook 'after-revert-hook #'smerge-smart-dwim t)
  )


(provide 'init-prog-vcs-diff)

;;; init-prog-vcs-diff.el ends here
