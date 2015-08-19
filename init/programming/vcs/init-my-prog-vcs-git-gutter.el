;;; init-my-prog-vcs-git-gutter.el --- init for Git gutter
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


;; ;; -----------------------------------------------
;; ;;  git-gutter.el vs git-gutter-fringe.el
;; ;;
;; ;; git-gutter.el 	git-gutter-fringe.el
;; ;; Work in tty frame 	    OK 	NG
;; ;; Work with linum-mode 	NG 	OK
;; ;; Show on right side 	    NG 	OK
;; ;; More configurable 	    OK 	NG
;; ;; ------------------------------------------------

;; (require 'git-gutter)

;; (setq git-gutter:disabled-modes '(asm-mode image-mode))

;; ;; update frequency: uncomment this when Emacs/GitGutter slows.
;; ;; (setq git-gutter:update-threshold 1)
;; ;; (setq git-gutter:update-hooks '(after-save-hook after-revert-hook))

;; (setq git-gutter:hide-gutter t)         ; Hide gutter if there are no changes
;; (setq git-gutter:diff-option "-w") ; Pass option to 'git diff' command: -w: ignore all spaces
;; (setq git-gutter:verbosity 0)           ; Log/Message Level


;; ;;; Usage:
;; ;; 'git-gutter:next-hunk :: Jump to next hunk
;; ;; 'git-gutter:previous-hunk :: Jump to previous hunk
;; ;; 'git-gutter:popup-hunk :: Popup current diff hunk
;; ;; 'git-gutter:stage-hunk :: Stage current hunk - `git add -p`
;; ;; 'git-gutter:revert-hunk :: Revert current hunk
;; ;; 'git-gutter :: Show changes from last commit or Update change information.
;; ;; 'git-gutter-toggle :: Toggle git-gutter
;; ;; 'git-gutter:update-all-windows :: update info in all visible windows.

;; ;;; keybindings
;; (define-key my-prog-vcs-map (kbd "m t") 'git-gutter:toggle)
;; ;; Jump to next/previous hunk
;; (define-key my-prog-vcs-map (kbd "m n") 'git-gutter:next-hunk)
;; (define-key my-prog-vcs-map (kbd "m p") 'git-gutter:previous-hunk)
;; ;; Stage current hunk
;; (define-key my-prog-vcs-map (kbd "m s") 'git-gutter:stage-hunk)
;; ;; show diff of current hunk
;; (define-key my-prog-vcs-map (kbd "m p") 'git-gutter:popup-hunk)
;; ;; Revert current hunk
;; (define-key my-prog-vcs-map (kbd "m r") 'git-gutter:revert-hunk)
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
;; ;; FIXME: this does not work at Emacs initially.
;; (set-face-attribute 'git-gutter:unchanged nil
;;                     :foreground nil :background nil
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
;; (diminish 'git-gutter-mode)


;; ;; ---------------------------
;; ;;; git-gutter-fringe.el is fringe version of of git-gutter.el.

;; ;; (require 'git-gutter-fringe)

;; ;; (set-face-foreground 'git-gutter-fr:modified "yellow")
;; ;; (set-face-foreground 'git-gutter-fr:added    "green")
;; ;; (set-face-foreground 'git-gutter-fr:deleted  "red")

;; ;; Please adjust fringe width if your own sign is too big.
;; ;; (setq-default left-fringe-width  20)
;; ;; (setq-default right-fringe-width 20)

;; ;; (fringe-helper-define 'git-gutter-fr:added nil
;; ;;   "...XX..."
;; ;;   "..X..X.."
;; ;;   ".X....X."
;; ;;   "X......X"
;; ;;   "X......X"
;; ;;   "XXXXXXXX"
;; ;;   "X......X"
;; ;;   "X......X"
;; ;;   "X......X")
;; ;;
;; ;; (fringe-helper-define 'git-gutter-fr:deleted nil
;; ;;   "XXXXXX.."
;; ;;   "XX....X."
;; ;;   "XX.....X"
;; ;;   "XX.....X"
;; ;;   "XX.....X"
;; ;;   "XX.....X"
;; ;;   "XX....X."
;; ;;   "XXXXXX..")
;; ;;
;; ;; (fringe-helper-define 'git-gutter-fr:modified nil
;; ;;   "XXXXXXXX"
;; ;;   "X..XX..X"
;; ;;   "X..XX..X"
;; ;;   "X..XX..X"
;; ;;   "X..XX..X"
;; ;;   "X..XX..X"
;; ;;   "X..XX..X"
;; ;;   "X..XX..X")


;; ;; (setq git-gutter-fr:side 'right-fringe)


;; [ git-gutter-plus / git-gutter+]

;; https://github.com/nonsequitur/git-gutter-plus

;;; Usage:
;;
;; Committing
;; The commit message buffer is based on git-commit-mode. Besides the default
;; git-commit-mode bindings, the following bindings are provided:
;;     C-c C-a toggles the option to amend the previous commit.
;;     C-c C-e toggles the option to allow an empty commit that includes no changes.
;;     C-c C-u toggles the option to edit the commit author.
;;     C-c C-d toggles the option to edit the commit date.
;;     M-p/M-n insert previous/next history commit message.
;; git-commit-ack is re-bound to C-c C-b.


;; (load "~/.emacs.d/el-get/git-gutter+/git-gutter+.el")
(require 'git-gutter+)


(eval-after-load 'git-gutter+
  '(progn
     ;;; keybindings
     (define-key my-prog-vcs-map (kbd "m t") 'git-gutter+-mode) ; Turn on/off in the current buffer
     (define-key my-prog-vcs-map (kbd "m T") 'global-git-gutter+-mode) ; Turn on/off globally
     ;; jump between hunks
     (define-key my-prog-vcs-map (kbd "m n") 'git-gutter+-next-hunk)
     (define-key my-prog-vcs-map (kbd "m p") 'git-gutter+-previous-hunk)
     ;; actions on hunks
     (define-key my-prog-vcs-map (kbd "m =") 'git-gutter+-show-hunk) ; diff
     (define-key my-prog-vcs-map (kbd "m D") 'git-gutter+-show-hunk) ; diff
     (define-key my-prog-vcs-map (kbd "m r") 'git-gutter+-revert-hunk)
     ;; stage hunk at point
     ;; if region is active, stage all hunk lines within the region.
     (define-key my-prog-vcs-map (kbd "m s") 'git-gutter+-stage-hunks)
     (define-key my-prog-vcs-map (kbd "m c") 'git-gutter+-commit)
     (define-key my-prog-vcs-map (kbd "m C") 'git-gutter+-stage-and-commit)
     (define-key my-prog-vcs-map (kbd "m u") 'git-gutter:update-all-windows)
     ))

(defun git-gutter+-show-hunk-at-point (&optional diffinfo)
  "Show DIFFINFO hunk at point at point."
  (interactive)
  (git-gutter+-awhen (or diffinfo
                        (git-gutter+-diffinfo-at-point))
    (let ((diff-lines "")
          (face nil))
      (with-current-buffer (get-buffer-create git-gutter+-popup-buffer)
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert (plist-get it :content))
        (insert "\n")
        (goto-char (point-min))
        (diff-mode)
        (view-mode)
        ;; workaround: buffer-substring doesn't returns text-properties (!?)
        ;; (setq diff-lines (buffer-substring (point-min) (point-max)))
        (while (not (eobp))
          (if (looking-at "@") (setq face 'diff-hunk-header))
          (if (looking-at "\+") (setq face 'diff-indicator-added))
          (if (looking-at "-") (setq face 'diff-indicator-removed))
          (setq diff-lines
                (concat
                 diff-lines
                 (propertize (concat (buffer-substring (point) (point-at-eol)) "\n")
                             'face face)))
          (forward-line 1)
          )
        )
      (momentary-string-display diff-lines (point-at-bol) 32)
      (discard-input))))

(define-key my-prog-vcs-map (kbd "m d") 'git-gutter+-show-hunk-at-point)


(setq git-gutter+-disabled-modes '(asm-mode image-mode))

(setq git-gutter+-hide-gutter t)         ; Hide gutter if there are no changes
(setq git-gutter+-diff-option "-w") ; Pass option to 'git diff' command: -w: ignore all spaces

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
;; FIXME: this does not work at Emacs initially.
(set-face-attribute 'git-gutter+-unchanged nil
                    :foreground nil :background nil
                    :weight 'bold
                    )
(set-face-foreground 'git-gutter+-separator "cyan")

(setq git-gutter+-added-sign "✚"
      git-gutter+-deleted-sign "✖"
      git-gutter+-modified-sign "Ϟ"
      git-gutter+-unchanged-sign nil
      ;; git-gutter+-window-width 2 ; multiple characters is ok.
      ;; | |, ┇, ┋ ⋮ ¦ ┊ ┆ │ │ ┃
      git-gutter+-separator-sign ""
      )


(global-git-gutter+-mode t)



;;; [ diff-hl ]

;; https://github.com/dgutov/diff-hl



(provide 'init-my-prog-vcs-git-gutter)

;;; init-my-prog-vcs-git-gutter.el ends here
