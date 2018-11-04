;;; init-prog-vcs-git-gutter.el --- init for Git gutter
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'git-gutter-prefix)
  (define-prefix-command 'git-gutter-prefix))
(global-set-key (kbd "M-g g") 'git-gutter-prefix)

;;; [ git-gutter ] -- Port of Sublime Text plugin GitGutter.

;; (use-package git-gutter
;;   :ensure t
;;   :ensure ivy
;;   :defer t
;;   :config
;;   (defun my-git-gutter-reshap (gutter)
;;     "Re-shape gutter for `ivy-read'."
;;     (let* ((linenum-start (aref gutter 3))
;;            (linenum-end (aref gutter 4))
;;            (target-line "")
;;            (target-linenum 1)
;;            (tmp-line "")
;;            (max-line-length 0))
;;       (save-excursion
;;         ;; find out the longest stripped line in the gutter
;;         (while (<= linenum-start linenum-end)
;;           (goto-line linenum-start)
;;           (setq tmp-line (replace-regexp-in-string "^[ \t]*" ""
;;                                                    (buffer-substring (line-beginning-position)
;;                                                                      (line-end-position))))
;;           (when (> (length tmp-line) max-line-length)
;;             (setq target-linenum linenum-start)
;;             (setq target-line tmp-line)
;;             (setq max-line-length (length tmp-line)))
;;
;;           (setq linenum-start (1+ linenum-start))))
;;       ;; build (key . linenum-start)
;;       (cons (format "%s %d: %s"
;;                     (if (eq 'deleted (aref gutter 1)) "-" "+")
;;                     target-linenum target-line)
;;             target-linenum)))
;;
;;   (defun my-git-gutter-goto ()
;;     (interactive)
;;     (if git-gutter:diffinfos
;;         (let* ((collection (mapcar 'my-git-gutter-reshap
;;                                    git-gutter:diffinfos)))
;;           (ivy-read "git-gutters:"
;;                     collection
;;                     :action (lambda (linenum)
;;                               (goto-line linenum))))
;;       (message "NO git-gutters!")))
;;
;;   (define-key prog-vcs-prefix (kbd "m g") 'my-git-gutter-goto)
;;   )

;; [ git-gutter+ ] -- Manage Git hunks straight from the buffer.

(use-package git-gutter+
  :ensure t
  :defer t
  :delight git-gutter+-mode
  :preface (setq git-gutter+-disabled-modes '(asm-mode image-mode))
  :init
  (autoload 'git-gutter+-turn-on "git-gutter+")
  (defun my/git-gutter+-turn-on ()
    (unless (and (buffer-file-name) (file-remote-p (buffer-file-name)))
      (git-gutter+-turn-on)))
  (add-hook 'prog-mode-hook #'my/git-gutter+-turn-on)
  :bind (:map git-gutter-prefix
              ("t" . git-gutter+-mode) ; Turn on/off in the current buffer
              ("T" . global-git-gutter+-mode) ; Turn on/off globally
              ;; jump between hunks
              ("n" . git-gutter+-next-hunk)
              ("p" . git-gutter+-previous-hunk)
              ;; actions on hunks
              ("d" . git-gutter+-show-hunk-inline-at-point)
              ("D" . git-gutter+-show-hunk) ; diff
              ("=" . git-gutter+-popup-hunk)
              ("r" . git-gutter+-revert-hunk)
              ;; stage hunk at point
              ;; if region is active, stage all hunk lines within the region.
              ("s" . git-gutter+-stage-hunks)
              ("c" . magit-commit-create)
              ("C" . git-gutter+-stage-and-commit)
              ("u" . git-gutter:update-all-windows)
              
              :map prog-vcs-prefix
              ("m t" . git-gutter+-mode) ; Turn on/off in the current buffer
              ("m T" . global-git-gutter+-mode) ; Turn on/off globally
              ;; jump between hunks
              ("m n" . git-gutter+-next-hunk)
              ("m p" . git-gutter+-previous-hunk)
              ;; actions on hunks
              ("m d" . git-gutter+-show-hunk-inline-at-point)
              ("m =" . git-gutter+-show-hunk) ; diff
              ("m D" . git-gutter+-show-hunk) ; diff
              ("m r" . git-gutter+-revert-hunk)
              ;; stage hunk at point
              ;; if region is active, stage all hunk lines within the region.
              ("m s" . git-gutter+-stage-hunks)
              ("m c" . git-gutter+-commit)
              ("m C" . git-gutter+-stage-and-commit)
              ("m u" . git-gutter:update-all-windows)
              )
  :config
  (add-to-list 'display-buffer-alist
               '("\\*git-gutter+-diff\\*" . (display-buffer-below-selected)))
  )

;;; [ diff-hl ] -- highlighting uncommitted changes with continuous fringe vertical block.

;; (use-package diff-hl
;;   :ensure t
;;   :defer t
;;   :commands (diff-hl-next-hunk diff-hl-previous-hunk diff-hl-diff-goto-hunk)
;;   :init
;;   (add-hook 'prog-mode-hook 'diff-hl-mode)
;;   (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
;;   :bind (:map git-gutter-prefix
;;               ("M-n" . diff-hl-next-hunk)
;;               ("M-p" . diff-hl-previous-hunk)
;;               ("M-=" . diff-hl-diff-goto-hunk))
;;   :config
;;   (setq vc-git-diff-switches '("--histogram"))
;;   (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;;; [ line-reminder ] -- Remind current line status by current buffer.

;; (use-package line-reminder
;;   :ensure t
;;   :init (global-line-reminder-mode t))



(provide 'init-prog-vcs-git-gutter)

;;; init-prog-vcs-git-gutter.el ends here
