;;; init-my-prog-vcs-git.el --- init Git for Emacs
;;
;;; Commentary:


;;; Usage:

;; - [X] help :: =[?]=
;; - [X] add to stage :: =[s, S, u, U]=
;; - [X] commit :: =[c]=
;; - [X] diff :: =[d]=
;; - [X] log
;;   - [X] file :: =[l]=
;;   - [X] repository :: =[L]=
;;   - [X] range :: =[l + rL]=
;; - [X] branch :: =[b]=
;;   - checkout
;;   - create
;;   - manage
;; - [X] pull :: =[F]=
;; - [X] push :: =[P]=


;;; Code:

;;; [ vc-git ] -- Git support backend

;;; Usage:
;; - [C-x v] -- prefix for vc-git.


;;; [ git.el ] -- front end wrapper for vc-git.

;;; Usage:
;; - [M-x git-help] -- get help of git.el
(require 'git)


;; [ git-modes ] -- front end wrapper for vc-git.
;;
;; The git-emacs package implements almost the same functionality, as the git.el
;; package, but it also has some improvements, mostly in the user interface
;;
;; GNU Emacs modes for various Git-related files
;; Features:
;; - gitconfig-mode     -- .gitconfig
;; - gitignore-mode     -- .gitignore
;; - gitattributes-mode -- .gitattributes
;; - git-commit-mode    -- git commit
;; - git-rebase-mode    -- git rebase -i (git-rebase-todo files)
;;
;;; Keybindings:
;; - [C-x g] -- prefix for global git-emacs keybindings.

(add-to-list 'auto-mode-alist
             '("\\.gitconfig$" . gitconfig-mode)
             '("\\.gitignore$" . gitignore-mode))


;;; [ git-emacs ] --- 

;;; The git-emacs package implements almost the same functionality, as the
;;; git.el package, but it also has some improvements, mostly in the user
;;; interface.

(require 'git-emacs)



;;; git-status

;; (setq git-status-modeline-decoration 'git-state-decoration-large-dot)



;;; [ Magit ]

;; Usage:
;; - [M-x magit-status] -- put you in Magit's status buffer.
;;   - press [?] in `magit-status'. press [q] in [?]help to exit.
;; - (C-h m in the status buffer) -- Read the short help for magit-mode.
;; - [C-h f magit RET] -- get Magit help.

(eval-after-load 'info
  '(progn (info-initialize)
          (add-to-list 'Info-directory-list "~/.emacs.d/el-get/magit/")))

(require 'magit)

(autoload 'magit-status "magit" nil t)

(setq magit-stage-all-confirm t
      magit-use-overlays t
      magit-diff-refine-hunk 'all
      magit-status-buffer-switch-function 'pop-to-buffer-same-window ; open magit status buffer in current window.
      )

;; TODO change to open magit-status in current window instead of overriding other windows. [default: 'pop-to-buffer].
(setq magit-status-buffer-switch-function 'pop-to-buffer)

;; add an extra newline to separate commit message from git commentary
(defun magit-commit-mode-init ()
  (when (looking-at "\n")
    (open-line 1)))
(add-hook 'magit-commit-mode-hook 'magit-commit-mode-init)
;; close popup when commiting
(defadvice git-commit-commit (after delete-window activate)
  (delete-window))

;; TODO add more common git action keybindings.
;; "g" for Git

;; status -- "s" for status.
;; [C-x v-] original is prefix for vc-.
(define-key vcs-map (kbd "v") 'magit-status)
(define-key vcs-git-map (kbd "s") 'magit-status)
;; add to stage -- "s"
;; FIXME this stage item, but can not stage file in current buffer.
;; (define-key vcs-git-map (kbd "a") 'magit-stage-item)
;; commit -- "c"
(define-key vcs-git-map (kbd "c") 'magit-commit)
;; diff - "d"
(define-key vcs-git-map (kbd "d") 'magit-diff)
;; file log - "l"
(define-key vcs-git-map (kbd "l") 'magit-log)
;; repository log - "L"
;; [M-n/p] to navigate.
(define-key vcs-git-map (kbd "L") 'magit-log-long)
;; grep - "g"
(define-key vcs-git-map (kbd "g") 'magit-grep)
;; checkout - "o"
(define-key vcs-git-map (kbd "o") 'magit-checkout) ; magit-checkout-branch-at-point


;; TODO: open magit window in current window, and without override other windows layout.


(set-face-attribute 'magit-item-highlight nil
                    :background "black"
                    ;; :box '(:color "deep pink" :line-width 2 :style nil)
                    )
(set-face-attribute 'magit-item-mark nil
                    :foreground "black"
                    :background "gray")
;; diff colors
(set-face-attribute 'magit-diff-none nil
                    :inherit 'diff-context
                    )
(set-face-attribute 'magit-diff-del nil
                    :inherit 'diff-removed
                    )
(set-face-attribute 'magit-diff-add nil
                    :inherit 'diff-added
                    )
(set-face-attribute 'magit-diff-hunk-header nil
                    :inherit 'diff-hunk-header
                    )
(set-face-attribute 'magit-diff-file-header nil
                    :inherit 'diff-file-header
                    )


;;; [ Egg ] (Emacs Got Git)
;;
;; The egg package is the fork of the magit package, described above. Main
;; difference from magit is an improvements in the users interface (example of
;; interface to work with history of changes you can see on the picture below)
;; and correct work on MS Windows, all other functional is almost the same as in
;; magit.

;; (require 'egg)


;; -----------------------------------------------
;;  git-gutter.el vs git-gutter-fringe.el
;;
;; git-gutter.el 	git-gutter-fringe.el
;; Work in tty frame 	    OK 	NG
;; Work with linum-mode 	NG 	OK
;; Show on right side 	    NG 	OK
;; More configurable 	    OK 	NG
;; ------------------------------------------------

(require 'git-gutter)

(setq git-gutter:disabled-modes '(asm-mode image-mode))

;; update frequency: uncomment this when Emacs/GitGutter slows.
;; (setq git-gutter:update-threshold 1)
;; (setq git-gutter:update-hooks '(after-save-hook after-revert-hook))

(setq git-gutter:hide-gutter t)         ; Hide gutter if there are no changes
(setq git-gutter:diff-option "-w") ; Pass option to 'git diff' command: -w: ignore all spaces
(setq git-gutter:verbosity 0)           ; Log/Message Level


;;; Usage:
;; 'git-gutter:next-hunk :: Jump to next hunk
;; 'git-gutter:previous-hunk :: Jump to previous hunk
;; 'git-gutter:popup-hunk :: Popup current diff hunk
;; 'git-gutter:stage-hunk :: Stage current hunk - `git add -p`
;; 'git-gutter:revert-hunk :: Revert current hunk
;; 'git-gutter :: Show changes from last commit or Update change information.
;; 'git-gutter-toggle :: Toggle git-gutter

;;; keybindings
(define-key vcs-map (kbd "m t") 'git-gutter:toggle)
;; Jump to next/previous hunk
(define-key vcs-map (kbd "m n") 'git-gutter:next-hunk)
(define-key vcs-map (kbd "m p") 'git-gutter:previous-hunk)
;; Stage current hunk
(define-key vcs-map (kbd "m s") 'git-gutter:stage-hunk)
;; show diff of current hunk
(define-key vcs-map (kbd "m p") 'git-gutter:popup-hunk)
;; Revert current hunk
(define-key vcs-map (kbd "m r") 'git-gutter:revert-hunk)
;; commit staged changes with [C-c v g c] which custom keybinding from magit function `magit-commit'.

;; multiple character is OK
(setq git-gutter:window-width 1
      git-gutter:modified-sign "Ϟ"
      git-gutter:unchanged-sign nil
      git-gutter:added-sign "✚"
      git-gutter:deleted-sign "✖"
      )

;; (setq git-gutter:window-width 2
;;       git-gutter:modified-sign "☁"
;;       git-gutter:unchanged-sign " "
;;       git-gutter:added-sign "☀"
;;       git-gutter:deleted-sign "☂"
;;       )

;; |, ┇, ┋ ⋮ ¦ ┊ ┆ │ ┃ ‡ † ‖
;; (setq git-gutter:separator-sign "│")
;; (set-face-foreground 'git-gutter:separator "yellow")

;; GitGutter signs
(set-face-attribute 'git-gutter:modified nil
                    :foreground "yellow"
                    :weight 'bold
                    )
(set-face-attribute 'git-gutter:added nil
                    :foreground "green"
                    :weight 'bold
                    )
(set-face-attribute 'git-gutter:deleted nil
                    :foreground "red"
                    :weight 'bold
                    )
;; FIXME: this does not work at Emacs initially.
(set-face-attribute 'git-gutter:unchanged nil
                    :foreground nil :background nil
                    :weight 'bold
                    )


(global-git-gutter-mode +1)
;; or
;; (add-hook 'ruby-mode-hook 'git-gutter-mode)

;; (setq git-gutter:lighter " GitGutter") ; minor mode name in modeline.
(diminish 'git-gutter-mode)



;; ---------------------------
;;; git-gutter-fringe.el is fringe version of of git-gutter.el.

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


;; [ git-gutter-plus ]

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



;; (require 'git-gutter+)

;; (global-git-gutter+-mode t)

;; ;;; keybindings
;; (define-key vcs-map (kbd "m t") 'git-gutter+-mode) ; Turn on/off in the current buffer
;; (define-key vcs-map (kbd "m T") 'global-git-gutter+-mode) ; Turn on/off globally

;; ;; NOTE: those keybindings conflict with `narrow' etc.
;; ;; (eval-after-load 'git-gutter+
;; ;;   '(progn
;; ;;      ;;; Jump between hunks
;; ;;      (define-key git-gutter+-mode-map (kbd "C-x n") 'git-gutter+-next-hunk)
;; ;;      (define-key git-gutter+-mode-map (kbd "C-x p") 'git-gutter+-previous-hunk)
;; ;;      ;;; Act on hunks
;; ;;      (define-key git-gutter+-mode-map (kbd "C-x v =") 'git-gutter+-show-hunk)
;; ;;      (define-key git-gutter+-mode-map (kbd "C-x r") 'git-gutter+-revert-hunks)
;; ;;      ;; Stage hunk at point.
;; ;;      ;; If region is active, stage all hunk lines within the region.
;; ;;      (define-key git-gutter+-mode-map (kbd "C-x t") 'git-gutter+-stage-hunks)
;; ;;      (define-key git-gutter+-mode-map (kbd "C-x c") 'git-gutter+-commit)
;; ;;      (define-key git-gutter+-mode-map (kbd "C-x C") 'git-gutter+-stage-and-commit)))

;; (eval-after-load 'git-gutter+
;;   '(progn
;;      ;; jump between hunks
;;      (define-key vcs-map (kbd "m n") 'git-gutter+-next-hunk)
;;      (define-key vcs-map (kbd "m p") 'git-gutter+-previous-hunk)
;;      ;; actions on hunks
;;      (define-key vcs-map (kbd "m =") 'git-gutter+-show-hunk)
;;      (define-key vcs-map (kbd "m r") 'git-gutter+-revert-hunk)
;;      ;; stage hunk at point
;;      ;; if region is active, stage all hunk lines within the region.
;;      (define-key vcs-map (kbd "m s") 'git-gutter+-stage-hunks)
;;      (define-key vcs-map (kbd "m c") 'git-gutter+-commit)
;;      (define-key vcs-map (kbd "m C") 'git-gutter+-stage-and-commit)
;;      ))

;; (setq git-gutter+-disabled-modes '(asm-mode image-mode))

;; (setq git-gutter+-hide-gutter t)         ; Hide gutter if there are no changes
;; (setq git-gutter+-diff-option "-w") ; Pass option to 'git diff' command: -w: ignore all spaces

;; ;; GitGutter signs
;; (set-face-attribute 'git-gutter+-modified nil
;;                     :foreground "yellow"
;;                     :weight 'bold
;;                     )
;; (set-face-attribute 'git-gutter+-added nil
;;                     :foreground "green"
;;                     :weight 'bold
;;                     )
;; (set-face-attribute 'git-gutter+-deleted nil
;;                     :foreground "red"
;;                     :weight 'bold
;;                     )
;; ;; FIXME: this does not work at Emacs initially.
;; (set-face-attribute 'git-gutter+-unchanged nil
;;                     :foreground nil :background nil
;;                     :weight 'bold
;;                     )
;; (set-face-foreground 'git-gutter+-separator "cyan")

;; (setq git-gutter+-added-sign "✚"
;;       git-gutter+-deleted-sign "✖"
;;       git-gutter+-modified-sign "Ϟ"
;;       git-gutter+-unchanged-sign nil
;;       ;; git-gutter+-window-width 2 ; multiple characters is ok.
;;       ;; |, ┇, ┋ ⋮ ¦ ┊ ┆ │ ┃ ‡ † ‖
;;       ;; git-gutter+-separator-sign "│"
;;       )



;;; [ diff-hl ]

;; https://github.com/dgutov/diff-hl


;;; [ MagitHub ] -- working with GitHub
;; (require 'magithub nil t)


;;; [ git-blame ]

;; (require 'git-blame)
;; OR
(autoload 'git-blame-mode "git-blame" "Minor mode for incremental blame for Git." t)


;;; [ mo-git-blame ] --- an interactive and iterative major mode for git blame.

;; That’s what ‘mo-git-blame’ tries to solve. It is a standalone mode that can
;; be used with any of the various Git version control modes. Here are a couple
;; of its features:
;;
;; Shows the output of ‘git blame’ and the file content side-by-side with syntax highlighting for the content
;; Show the log messages for the current revision or the revision in the current line
;; Show the diff (via ‘git show’) for the current revision or the revision in the current line
;; Show the file content for the revision in the current line
;; Call ‘git blame’ for the same file for the revision in the current line

(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)


;;; [ gitsum ]


;;; [ egit ]
;; is an Emacs Git history interface intended to be similar to qgit or gitk.
;; https://github.com/jimhourihan/egit/tree/master

;; (autoload 'egit "egit" "Emacs git history" t)
;; (autoload 'egit-file "egit" "Emacs git history file" t)
;; (autoload 'egit-dir "egit" "Emacs git history directory" t)


;;; [ git-timemachine ] -- Step through historic versions of git controlled file using everyone's favourite editor.

;; Usage:
;; - [M-x git-timemachine ]
;;
;; Use the following keys to navigate historic version of the file
;;
;; p Visit previous historic version
;; n Visit next historic version
;; w Copy the hash of the current historic version
;; q Exit the time machine.

(require 'git-timemachine)
(define-key 'vcs-git-map (kbd "C-h") 'git-timemachine)



(provide 'init-my-prog-vcs-git)

;;; init-my-prog-vcs-git.el ends here
