;;; init-my-prog-vcs-git-gutter.el --- init for Git gutter
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ git-gutter ]

(use-package git-gutter
  :ensure t)

;; (setq git-gutter:disabled-modes '(asm-mode image-mode))

;; ;; update frequency: uncomment this when Emacs/GitGutter slows.
;; ;; (setq git-gutter:update-threshold 1)
;; ;; (setq git-gutter:update-hooks '(after-save-hook after-revert-hook))

;; (setq git-gutter:hide-gutter t)         ; Hide gutter if there are no changes
;; (setq git-gutter:diff-option "-w") ; Pass option to 'git diff' command: -w: ignore all spaces
;; (setq git-gutter:verbosity 0)           ; Log/Message Level

;; ;;; keybindings
;; (define-key my-prog-vcs-map (kbd "m t") 'git-gutter:toggle)
;; ;; Jump to next/previous hunk
;; (define-key my-prog-vcs-map (kbd "m n") 'git-gutter:next-hunk)
;; (define-key my-prog-vcs-map (kbd "m p") 'git-gutter:previous-hunk)
;; ;; Mark current hunk
;; (define-key my-prog-vcs-map (kbd "m SPC") 'git-gutter:mark-hunk)
;; ;; Stage current hunk
;; (define-key my-prog-vcs-map (kbd "m s") 'git-gutter:stage-hunk)
;; ;; show diff of current hunk
;; (define-key my-prog-vcs-map (kbd "m p") 'git-gutter:popup-hunk)
;; ;; Revert current hunk
;; (define-key my-prog-vcs-map (kbd "m r") 'git-gutter:revert-hunk)
;; ;; show current buffer's statistic
;; (define-key my-prog-vcs-map (kbd "m S") 'git-gutter:statistic)
;; ;; commit staged changes with [C-c v g c] which custom keybinding from magit function `magit-commit'.

;; ;; multiple character is OK
;; (setq git-gutter:window-width 1
;;       git-gutter:modified-sign "Ϟ"
;;       git-gutter:unchanged-sign nil
;;       git-gutter:added-sign "✚"
;;       git-gutter:deleted-sign "✖"
;;       )

;; ;; (setq git-gutter:window-width 2
;; ;;       git-gutter:modified-sign "☁"
;; ;;       git-gutter:unchanged-sign " "
;; ;;       git-gutter:added-sign "☀"
;; ;;       git-gutter:deleted-sign "☂"
;; ;;       )

;; ;; |, ┇, ┋ ⋮ ¦ ┊ ┆ │ ┃ ‡ † ‖
;; ;; (setq git-gutter:separator-sign "│")
;; ;; (set-face-foreground 'git-gutter:separator "yellow")

;; ;; GitGutter signs
;; (set-face-attribute 'git-gutter:modified nil
;;                     :foreground "yellow"
;;                     :weight 'bold
;;                     )
;; (set-face-attribute 'git-gutter:added nil
;;                     :foreground "green"
;;                     :weight 'bold
;;                     )
;; (set-face-attribute 'git-gutter:deleted nil
;;                     :foreground "red"
;;                     :weight 'bold
;;                     )
;; (set-face-attribute 'git-gutter:unchanged nil
;;                     :weight 'bold
;;                     )

;; ;; (add-to-list 'git-gutter:update-hooks '(after-save-hook
;; ;;                                         after-revert-hook
;; ;;                                         find-file-hook
;; ;;                                         after-change-major-mode-hook
;; ;;                                         text-scale-mode-hook
;; ;;                                         magit-revert-buffer-hook
;; ;;                                         ))

;; (add-hook 'linum-mode-hook 'git-gutter:linum-setup)

;; (global-git-gutter-mode +1)

;; (setq git-gutter:lighter " GitGutter") ; minor mode name in modeline.


;; enhance git-gutter with `ivy-mode'.
(use-package ivy
  :ensure t)

(defun my-git-gutter-reshap (gutter)
  "Re-shape gutter for `ivy-read'."
  (let* ((linenum-start (aref gutter 3))
         (linenum-end (aref gutter 4))
         (target-line "")
         (target-linenum 1)
         (tmp-line "")
         (max-line-length 0))
    (save-excursion
      ;; find out the longest stripped line in the gutter
      (while (<= linenum-start linenum-end)
        (goto-line linenum-start)
        (setq tmp-line (replace-regexp-in-string "^[ \t]*" ""
                                                 (buffer-substring (line-beginning-position)
                                                                   (line-end-position))))
        (when (> (length tmp-line) max-line-length)
          (setq target-linenum linenum-start)
          (setq target-line tmp-line)
          (setq max-line-length (length tmp-line)))

        (setq linenum-start (1+ linenum-start))))
    ;; build (key . linenum-start)
    (cons (format "%s %d: %s"
                  (if (eq 'deleted (aref gutter 1)) "-" "+")
                  target-linenum target-line)
          target-linenum)))

(defun my-git-gutter-goto ()
  (interactive)
  (if git-gutter:diffinfos
      (let* ((collection (mapcar 'my-git-gutter-reshap
                                 git-gutter:diffinfos)))
        (ivy-read "git-gutters:"
                  collection
                  :action (lambda (linenum)
                            (goto-line linenum))))
    (message "NO git-gutters!")))

(define-key my-prog-vcs-map (kbd "m g") 'my-git-gutter-goto)


;;; [ git-gutter-fringe.el ] -- fringe version of of git-gutter.el.

;; (require 'git-gutter-fringe)

;; (set-face-foreground 'git-gutter-fr:modified "yellow")
;; (set-face-foreground 'git-gutter-fr:added    "green")
;; (set-face-foreground 'git-gutter-fr:deleted  "red")

;; Please adjust fringe width if your own sign is too big.
;; (setq-default left-fringe-width  20)
;; (setq-default right-fringe-width 20)

;; (fringe-helper-define 'git-gutter-fr:added nil
;;   "...XX..."
;;   "..X..X.."
;;   ".X....X."
;;   "X......X"
;;   "X......X"
;;   "XXXXXXXX"
;;   "X......X"
;;   "X......X"
;;   "X......X")
;;
;; (fringe-helper-define 'git-gutter-fr:deleted nil
;;   "XXXXXX.."
;;   "XX....X."
;;   "XX.....X"
;;   "XX.....X"
;;   "XX.....X"
;;   "XX.....X"
;;   "XX....X."
;;   "XXXXXX..")
;;
;; (fringe-helper-define 'git-gutter-fr:modified nil
;;   "XXXXXXXX"
;;   "X..XX..X"
;;   "X..XX..X"
;;   "X..XX..X"
;;   "X..XX..X"
;;   "X..XX..X"
;;   "X..XX..X"
;;   "X..XX..X")


;; (setq git-gutter-fr:side 'right-fringe)


;; [ git-gutter-plus / git-gutter+]

;;; Usage:
;;
;; Committing
;; The commit message buffer is based on git-commit-mode. Besides the default
;; git-commit-mode bindings, the following bindings are provided:
;;
;;- [C-c C-a] :: toggles the option to amend the previous commit.
;;- [C-c C-e] :: toggles the option to allow an empty commit that
;;               includes no changes.
;;- [C-c C-u] :: toggles the option to edit the commit author.
;;- [C-c C-d] :: toggles the option to edit the commit date.
;;- [M-p/M-n] :: insert previous/next history commit message.
;;
;; git-commit-ack is re-bound to C-c C-b.

(use-package git-gutter+
  :ensure t
  :init
  (progn
    (define-key my-prog-vcs-map (kbd "m t") 'git-gutter+-mode) ; Turn on/off in the current buffer
    (define-key my-prog-vcs-map (kbd "m T") 'global-git-gutter+-mode) ; Turn on/off globally
    ;; jump between hunks
    (define-key my-prog-vcs-map (kbd "m n") 'git-gutter+-next-hunk)
    (define-key my-prog-vcs-map (kbd "m p") 'git-gutter+-previous-hunk)
    ;; actions on hunks
    (define-key my-prog-vcs-map (kbd "m d") 'git-gutter+-show-hunk-inline-at-point)
    (define-key my-prog-vcs-map (kbd "m =") 'git-gutter+-show-hunk) ; diff
    (define-key my-prog-vcs-map (kbd "m D") 'git-gutter+-show-hunk) ; diff
    (define-key my-prog-vcs-map (kbd "m r") 'git-gutter+-revert-hunk)
    ;; stage hunk at point
    ;; if region is active, stage all hunk lines within the region.
    (define-key my-prog-vcs-map (kbd "m s") 'git-gutter+-stage-hunks)
    (define-key my-prog-vcs-map (kbd "m c") 'git-gutter+-commit)
    (define-key my-prog-vcs-map (kbd "m C") 'git-gutter+-stage-and-commit)
    (define-key my-prog-vcs-map (kbd "m u") 'git-gutter:update-all-windows))
  
  :config  
  (setq git-gutter+-disabled-modes '(asm-mode image-mode)
        ;; hide gutter if there are no changes
        git-gutter+-hide-gutter t
        ;; pass option to 'git diff' command: -w: ignore all spaces
        git-gutter+-diff-option "-w")

  (setq git-gutter+-added-sign "✚"
        git-gutter+-deleted-sign "✖"
        git-gutter+-modified-sign "Ϟ"
        git-gutter+-unchanged-sign nil
        ;; git-gutter+-window-width 2 ; multiple characters is ok.
        ;; | |, ┇, ┋ ⋮ ¦ ┊ ┆ │ │ ┃
        git-gutter+-separator-sign ""
        )
  
  ;; GitGutter signs
  (set-face-attribute 'git-gutter+-modified nil
                      :foreground "yellow"
                      :weight 'normal
                      :height 90
                      )
  (set-face-attribute 'git-gutter+-added nil
                      :foreground "green"
                      :weight 'normal
                      :height 90
                      )
  (set-face-attribute 'git-gutter+-deleted nil
                      :foreground "red"
                      :weight 'normal
                      :height 90
                      )
  (set-face-attribute 'git-gutter+-unchanged nil
                      )
  (set-face-foreground 'git-gutter+-separator "cyan")

  (global-git-gutter+-mode t)
  )


;;; [ diff-hl ]

;; https://github.com/dgutov/diff-hl



(provide 'init-my-prog-vcs-git-gutter)

;;; init-my-prog-vcs-git-gutter.el ends here
