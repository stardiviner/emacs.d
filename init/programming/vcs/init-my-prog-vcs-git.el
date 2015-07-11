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

;; git-modeline

;;; Usage:
;; (git--install-state-mark-modeline 'modified)
;; (git--uninstall-state-mark-modeline)
;; (git--update-all-state-marks)

;; (setq git-status-modeline-decoration 'git-state-decoration-small-dot)


;;; [ Magit ]

;; Usage:
;;
;; - [M-x magit-status] -- put you in Magit's status buffer.
;;   - press [?] in `magit-status'. press [q] in [?]help to exit.
;; - (C-h m in the status buffer) -- Read the short help for magit-mode.
;; - [C-h f magit RET] -- get Magit help.
;; - info magit & magit-popup
;; - [$] :: `magit-process-buffer' :: show git commands output.

(require 'magit)

(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list "~/.emacs.d/el-get/magit/Documentation/"))


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
(define-key my-prog-vcs-map (kbd "v") 'magit-status)
(define-key my-prog-vcs-git-map (kbd "v") 'magit-status)
;; add to stage -- "s"
(define-key my-prog-vcs-git-map (kbd "s") 'magit-stage)
;; commit -- "c"
(define-key my-prog-vcs-git-map (kbd "c") 'magit-commit)
;; diff - "d"
(define-key my-prog-vcs-git-map (kbd "d") 'magit-diff)
;; log - "l"
(define-key my-prog-vcs-git-map (kbd "l") 'magit-log)
;; file log
(define-key my-prog-vcs-git-map (kbd "f") 'magit-file-log)
;; repository log - "L"
;; [M-n/p] to navigate.
(define-key my-prog-vcs-git-map (kbd "L") 'magit-log-long)
;; grep - "g"
(define-key my-prog-vcs-git-map (kbd "g") 'magit-grep)
;; checkout - "o"
(define-key my-prog-vcs-git-map (kbd "o") 'magit-checkout) ; magit-checkout-branch-at-point
;; bisect -- "b"
(define-key my-prog-vcs-git-map (kbd "b") 'magit-bisect)
;; blame -- "h"
(define-key my-prog-vcs-git-map (kbd "h") 'magit-blame)


;; TODO: open magit window in current window, and without override other windows layout.


;;; Magit Faces
;; cursor select
(set-face-attribute 'magit-item-highlight nil
                    :background "black"
                    )
;; mark region
(set-face-attribute 'magit-item-mark nil
                    :foreground "black"
                    :background "gray")
;; branch
(set-face-attribute 'magit-branch nil
                    :foreground "cyan" :background "black"
                    :weight 'bold
                    :box '(:line-width -1)
                    )
;; log
(set-face-attribute 'magit-log-sha1 nil
                    :foreground "#FF80FF" :background " "
                    :weight 'bold
                    )
;; section
(set-face-attribute 'magit-section-title nil
                    :foreground "sky blue" :background "#222222"
                    :reverse-video nil
                    :weight 'bold
                    :box '(:color "cyan" :line-width 1)
                    )
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


;;; [ magit-filenotify ] -- Refresh status buffer when git tree changes

;;; This module for magit provides an auto update mode for the status buffer. It
;;; uses the file notification support that was added to Emacs 24.4.
;;;
;;; This is an updated version of the obsolete contrib/magit-inotify.el mode.

;;; Usage:
;;
;; Activate the mode inside the magit-status buffer by calling
;; [M-x magit-filenotify-mode]
;; Repeat the same step to deactivate it again.

;; To always enable the mode when opening the magit-status buffer.
;; add magit-filenotify-mode to the magit-status-mode-hook.
;;


;;; [ magit-workflow ] -- Git Flow plugin for magit

;;; Usage:
;;
;; - [C-f] in magit status buffer and you will be presented with gitflow popup menu.
;; - All gitflow commands are also accessible through the Magit/Extensions/GitFlow pop-down menu.

(require 'magit-gitflow)

(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)


;;; [ Egg ] (Emacs Got Git)
;;
;; The egg package is the fork of the magit package, described above. Main
;; difference from magit is an improvements in the users interface (example of
;; interface to work with history of changes you can see on the picture below)
;; and correct work on MS Windows, all other functional is almost the same as in
;; magit.

;; (require 'egg)



;;; [ magithub ] -- working with GitHub

(require 'magithub)


;;; [ magit-gh-pulls ] -- Magit plugin for dealing with GitHub pull requests.

;;; Usage:
;;
;; in `magit-status' window.
;;
;;   - [# g g] :: refresh the list of pull requests.
;;
;;   - [# g f] :: fetch the commits associated with the PR.
;;
;;   - [# g b] :: press the key on the PR to create a "topic branch" for this PR.
;;                After testing the PR you can merge it back into your branch using Magit.
;;
;;   - [# g m] :: merge the PR on top of the currently checked out branch.
;;                This is convenient if pull request can be merged by
;;                fast-forwarding and no testing is needed (or you can test from
;;                your branch directly). A nice benefit of this approach over
;;                merging from Github interface is that in case of FF no merge
;;                commit is produced, so history stays nice and linear.

(require 'magit-gh-pulls)

(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)


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

;; (require 'git-timemachine)
;; (define-key 'my-prog-vcs-git-map (kbd "C-h") 'git-timemachine)



;;; [ git-messenger ] -- popup commit message at current line.

;;; This is useful when you want to know why this line was changed.

;; `git-messenger-map' :: keybinding on git-messenger popup.

(require 'git-messenger)

(setq git-messenger:show-detail t ; always show detail message.
      ;; git-messenger:handled-backends '(git svn)
      )

(define-key my-prog-vcs-map (kbd "p") 'git-messenger:popup-message)
(define-key git-messenger-map (kbd "m") 'git-messenger:copy-message)
;; enable `magit-commit-mode' after typing 's', 'S', 'd'
(add-hook 'git-messenger:popup-buffer-hook 'magit-commit-mode)



(require 'init-my-prog-vcs-git-gutter)



(provide 'init-my-prog-vcs-git)

;;; init-my-prog-vcs-git.el ends here
