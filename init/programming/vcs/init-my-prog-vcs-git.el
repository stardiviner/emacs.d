;;; init-my-prog-vcs-git.el --- init Git for Emacs
;;
;;; Commentary:

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
             '("\\.gitignore$" . gitignore-mode)
             ;; FIXME: '("\\.gitattributes$" . gitattributes-mode)
             )


;;; [ git-emacs ] --- 

;;; The git-emacs package implements almost the same functionality, as the
;;; git.el package, but it also has some improvements, mostly in the user
;;; interface.

; (require 'git-emacs)

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

;;; completion
;; `magit-builtin-completing-read', `magit-ido-completing-read'
;; (setq magit-completing-read-function 'magit-builtin-completing-read)

(setq magit-delete-by-moving-to-trash t)

;; auto refresh & revert
(setq magit-revert-buffers 'usage)

;;; wip (work-in-process) modes
;; - [M-x magit-wip-commit]
;; NOTE: These modes are not enabled by default because of performance concerns.
;;
;; (setq magit-wip-after-save-mode t
;;       magit-wip-after-apply-mode t
;;       ;; magit-wip-after-save-local-mode-lighter
;;       ;; magit-wip-after-apply-mode-lighter
;;       ;; magit-wip-before-change-mode-lighter
;;       magit-wip-namespace "refs/wip/"
;;       )
;;
;; (add-to-list 'magit-no-confirm 'safe-with-wip)

(define-key magit-status-mode-map (kbd "H") 'magit-blame-popup)
(define-key my-prog-vcs-git-map (kbd "b") 'magit-blame-popup)
(define-key my-prog-vcs-map (kbd "F") 'magit-log-buffer-file) ; show log for current buffer visiting file.

(setq magit-diff-auto-show '(commit stage-all log-oneline log-select blame-follow)
      magit-diff-highlight-hunk-body t
      magit-diff-highlight-indentation nil
      magit-diff-highlight-trailing t
      magit-diff-paint-whitespace t
      magit-diff-refine-hunk 'all)

;; push
;; (setq magit-push-always-verify 'nag)


;;; Magit Git

;; (setq magit-git-global-arguments)


;;; Magit Popup

;; This option controls whether the section which lists the commands that are
;; common to all popups is initially show. We recommend you set this to nil -
;; after you have memorized that it can be shown on demand using [C-t].
;;
(setq magit-popup-show-common-commands nil) ; [C-t] to toggle

;; (setq magit-popup-use-prefix-argument 'disabled)


;;; Magit Faces

;; file
(set-face-attribute 'magit-diff-file-heading nil
                    :weight 'normal)
(set-face-attribute 'magit-diff-file-heading-highlight nil ; current diff file headings.
                    :weight 'normal)
(set-face-attribute 'magit-diff-file-heading-selection nil ; current select region
                    :weight 'normal)

;; section
(set-face-attribute 'magit-section-heading nil
                    :foreground "sky blue" :background "#222222"
                    :box '(:color "cyan" :line-width 1)
                    :weight 'bold)
(set-face-attribute 'magit-section-highlight nil ; current section (current selected thing)
                    :background (color-darken-name (face-background 'default) 3)
                    ;; :box '(:color "black" :line-width -1)
                    )

;; branch
(set-face-attribute 'magit-branch-local nil
                    :foreground "#8BEEFF" :background "#1B8194"
                    :box '(:color "#8BEEFF" :line-width -1)
                    :weight 'normal)
(set-face-attribute 'magit-branch-remote nil
                    :foreground "deep pink" :background "dark red"
                    :box '(:color "deep pink" :line-width -1)
                    :weight 'normal)

;; hash
(set-face-attribute 'magit-hash nil
                    :foreground "orange")

;; signature
(set-face-attribute 'magit-signature-untrusted nil
                    :foreground "dark green")

;; diff colors



;;; with-editor

;; The commands with-editor-async-shell-command and with-editor-shell-command
;; are intended as drop in replacements for async-shell-command and
;; shell-command. They automatically export $EDITOR making sure the executed
;; command uses the current Emacs instance as "the editor". With a prefix
;; argument these commands prompt for an alternative environment variable such
;; as $GIT_EDITOR.

(define-key (current-global-map)
  [remap async-shell-command] 'with-editor-async-shell-command)
(define-key (current-global-map)
  [remap shell-command] 'with-editor-shell-command)

;; The command with-editor-export-editor exports $EDITOR or another such
;; environment variable in shell-mode, term-mode and eshell-mode buffers. Use
;; this Emacs command before executing a shell command which needs the editor
;; set, or always arrange for the current Emacs instance to be used as editor by
;; adding it to the appropriate mode hooks:

(add-hook 'shell-mode-hook  'with-editor-export-editor)
(add-hook 'term-mode-hook   'with-editor-export-editor)
(add-hook 'eshell-mode-hook 'with-editor-export-editor)

(add-hook 'shell-mode-hook
          (apply-partially 'with-editor-export-editor "GIT_EDITOR"))
(add-hook 'shell-mode-hook 'with-editor-export-git-editor)


;;; Keybindings

;; status -- "s" for status.
;; [C-x v-] original is prefix for vc-.
(define-key my-prog-vcs-map (kbd "v") 'magit-status)
(define-key my-prog-vcs-git-map (kbd "v") 'magit-status)
;; add to stage -- "s"
(define-key my-prog-vcs-git-map (kbd "s") 'magit-stage)
;; commit -- "c"
(define-key my-prog-vcs-git-map (kbd "c") 'magit-commit)
(define-key my-prog-vcs-git-map (kbd "C") 'magit-commit-amend)
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
(define-key my-prog-vcs-git-map (kbd "B") 'magit-bisect)
;; blame -- "h"
(define-key my-prog-vcs-git-map (kbd "b") 'magit-blame)


;; for Magit auto-complete
;; TODO: how to use this in Magit.
;; (setq magit-repository-directories '("~/code/" "~/Git/")
;;       magit-repository-directories-depth 4)

;;; Magit Faces
;;
;; - [M-x customize-group magit-faces]

;; TODO: set remote branch face like this:
;; https://emacs.stackexchange.com/questions/10975/customize-face-magit-item-highlight-properly


;;; [ magit-find-file ]




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
;; (add-hook 'magit-status-mode-hook 'magit-filenotify-mode)


;;; [ magit-workflow ] -- Git Flow plugin for magit

;;; Usage:
;;
;; - [C-f] in magit status buffer and you will be presented with gitflow popup menu.
;; - All gitflow commands are also accessible through the Magit/Extensions/GitFlow pop-down menu.

(add-hook 'magit-mode-hook
          (lambda ()
            (require 'magit-gitflow)
            (when (fboundp 'turn-on-magit-gitflow)
              (turn-on-magit-gitflow))))


;;; [ Egg ] (Emacs Got Git)
;;
;; The egg package is the fork of the magit package, described above. Main
;; difference from magit is an improvements in the users interface (example of
;; interface to work with history of changes you can see on the picture below)
;; and correct work on MS Windows, all other functional is almost the same as in
;; magit.

;; (require 'egg)



;;; [ magithub ] -- working with GitHub

;; (require 'magithub)


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

;; (require 'magit-gh-pulls)
;;
;; (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)


;;; [ magit-gerrit ] -- code review tool Gerrit for Magit.

;;; Usage:
;;
;; Workflow
;;
;;     - Check out branch, make changes, and commit...
;;     - Gerrit Push Commit for Code Review => T P
;;     - Gerrit Add Reviewer => T A (optional)
;;     - Wait for code review and verification (approvals updated in magit-status)
;;     - Gerrit Submit Review => T S


;; (require 'magit-gerrit)
;;
;; ;; if remote url is not using the default gerrit port and
;; ;; ssh scheme, need to manually set this variable
;; (setq-default magit-gerrit-ssh-creds "myid@gerrithost.org")
;;
;; ;; if necessary, use an alternative remote instead of 'origin'
;; (setq-default magit-gerrit-remote "gerrit")


;;; [ git-messenger ] -- popup commit message at current line.

;;; This is useful when you want to know why this line was changed.

;; `git-messenger-map' :: keybinding on git-messenger popup.

(setq git-messenger:show-detail t ; always show detail message.
      ;; git-messenger:handled-backends '(git svn)
      )

(define-key my-prog-vcs-map (kbd "p") 'git-messenger:popup-message)
(use-package git-messenger
  :config
  (define-key git-messenger-map (kbd "m") 'git-messenger:copy-message)
  ;; enable `magit-commit-mode' after typing 's', 'S', 'd'
  (add-hook 'git-messenger:popup-buffer-hook 'magit-commit-mode)
  )



(require 'init-my-prog-vcs-git-gutter)



(provide 'init-my-prog-vcs-git)

;;; init-my-prog-vcs-git.el ends here
