;;; init-my-prog-git.el --- init Git for Emacs
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
;; - [C-x v] -- prefix for vc-git.


;;; [ git.el ] -- front end wrapper for vc-git.
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


;;; [ git-emacs ]
;; which provides status bar icons for git status.
;;
;; The git-emacs package implements almost the same functionality, as the git.el
;; package, but it also has some improvements, mostly in the user interface.

;; load at startup
;; (unless (package-installed-p 'git-emacs)
;;   (package-install 'git-emacs))
;; FIXME (require 'git-emacs)
;; (unless (package-installed-p 'git-status)
;;   (package-install 'git-status))
;; FIXME (require 'git-status)

;; autoload
;; (require 'git-emacs-autoloads)

;; (setq git-status-modeline-decoration 'git-state-decoration-large-dot)


;;; [ Magit ]
;; Usage:
;; - [M-x magit-status] -- put you in Magit's status buffer.
;;   - press [?] in `magit-status'. press [q] in [?]help to exit.
;; - (C-h m in the status buffer) -- Read the short help for magit-mode.
;; - [C-h f magit RET] -- get Magit help.

(require 'magit)

(autoload 'magit-status "magit" nil t)

(setq magit-stage-all-confirm t)

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
 ; [C-x v-] original is prefix for vc-.
(define-key global-map (kbd "C-x v") 'magit-status)
(define-key global-map (kbd "C-c v s") 'magit-status)
;; add to stage -- "s"
;; FIXME this stage item, but can not stage file in current buffer.
;; (define-key global-map (kbd "C-c v a") 'magit-stage-item)
;; commit -- "c"
(define-key global-map (kbd "C-c v c") 'magit-commit)
;; diff - "d"
(define-key global-map (kbd "C-c v d") 'magit-diff)
;; file log - "l"
(define-key global-map (kbd "C-c v l") 'magit-log)
;; repository log - "L"
;; [M-n/p] to navigate.
(define-key global-map (kbd "C-c v L") 'magit-log-long)
;; grep - "g"
(define-key global-map (kbd "C-c v g") 'magit-grep)
;; checkout - "o"
(define-key global-map (kbd "C-c v o") 'magit-checkout) ; magit-checkout-branch-at-point


;; TODO open magit window in current window, and without override other windows layout.


(set-face-attribute 'magit-item-highlight nil
                    :background "black"
                    ;; :box '(:color "deep pink" :line-width 2 :style nil)
                    )
(set-face-attribute 'magit-item-mark nil
                    :foreground "black"
                    :background "gray")


;;; [ MagitHub ] -- working with GitHub
;; (require 'magithub nil t)


;;; [ Egg ] (Emacs Got Git)
;;
;; The egg package is the fork of the magit package, described above. Main
;; difference from magit is an improvements in the users interface (example of
;; interface to work with history of changes you can see on the picture below)
;; and correct work on MS Windows, all other functional is almost the same as in
;; magit.

;; (require 'egg)


;;; [ git-blame ]

;; (require 'git-blame)
;; OR
(autoload 'git-blame-mode "git-blame" "Minor mode for incremental blame for Git." t)


;;; [ mo-git-blame ]
;;  is an interactive and iterative major mode for git blame.

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


;;; gitsum


;;; [ egit ]
;; is an Emacs Git history interface intended to be similar to qgit or gitk.
;; https://github.com/jimhourihan/egit/tree/master

;; (autoload 'egit "egit" "Emacs git history" t)
;; (autoload 'egit-file "egit" "Emacs git history file" t)
;; (autoload 'egit-dir "egit" "Emacs git history directory" t)



(provide 'init-my-prog-git)

;;; init-my-prog-git.el ends here
