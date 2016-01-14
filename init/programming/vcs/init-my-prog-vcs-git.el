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
             '("\\.gitconfig\\'" . gitconfig-mode)
             '("\\.gitignore\\'" . gitignore-mode)
             ;; FIXME: '("\\.gitattributes$" . gitattributes-mode)
             )


;;; [ git-emacs ] --- 

;;; The git-emacs package implements almost the same functionality, as the
;;; git.el package, but it also has some improvements, mostly in the user
;;; interface.

;; (require 'git-emacs)

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


;;; auto popup error buffer when magit process error.


;;; Magit Git

;; (setq magit-git-global-arguments)


;;; Magit Popup

;; This option controls whether the section which lists the commands that are
;; common to all popups is initially show. We recommend you set this to nil -
;; after you have memorized that it can be shown on demand using [C-t].
;;
(setq magit-popup-show-common-commands nil) ; [C-t] to toggle

;; (setq magit-popup-use-prefix-argument 'disabled)


;;; Magit Buffers

;; 'magit-display-buffer-traditional, 'display-buffer, 'function.
(setq magit-display-buffer-function 'display-buffer)

;; let magit status buffer display in current window.
(add-to-list 'display-buffer-alist
             '("\\`\\*magit:.*\\'"
               (display-buffer-reuse-window
                display-buffer-same-window)
               ))


;;; [ revision ]

;; (setq magit-revision-headers-format "")
;; (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))


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
(set-face-attribute 'magit-diff-added nil
                    :background "#ddffdd"
                    :foreground "#22aa22")
(set-face-attribute 'magit-diff-removed nil
                    :background "#ffdddd"
                    :foreground "#aa2222")
(set-face-attribute 'magit-diff-our nil
                    :background "#ffffcc"
                    :foreground "#aaaa11")
(set-face-attribute 'magit-diff-base nil
                    :background "#ffffcc"
                    :foreground "#aaaa11")
(set-face-attribute 'magit-diff-their nil
                    :background "#ffffcc"
                    :foreground "#aaaa11")

;; (set-face-attribute 'magit-diff-removed-highlight nil
;;                     )
;; (set-face-attribute 'magit-diff-added-highlight nil
;;                     )
;; (set-face-attribute 'magit-diff-our-highlight nil
;;                     )
;; (set-face-attribute 'magit-diff-base-highlight nil
;;                     )
;; (set-face-attribute 'magit-diff-their-highlight nil
;;                     )



;;; [ with-editor ]

;; [M-x with-editor-debug]

;; Use the Emacsclient as `$EDITOR' of child processes, making sure
;; they know how to call home.  For remote processes a substitute is
;; provided, which communicates with Emacs on stdout instead of using
;; a socket as the Emacsclient does.

;; The commands with-editor-async-shell-command and with-editor-shell-command
;; are intended as drop in replacements for async-shell-command and
;; shell-command. They automatically export $EDITOR making sure the executed
;; command uses the current Emacs instance as "the editor". With a prefix
;; argument these commands prompt for an alternative environment variable such
;; as $GIT_EDITOR. To always use these variants add this to your init file:.

;; (define-key (current-global-map)
;;   [remap async-shell-command] 'with-editor-async-shell-command)
;; (define-key (current-global-map)
;;   [remap shell-command] 'with-editor-shell-command)

;; Alternatively use the global `shell-command-with-editor-mode',
;; which always sets `$EDITOR' for all Emacs commands which ultimately
;; use `shell-command' to asynchronously run some shell command.

;; The command `with-editor-export-editor' exports `$EDITOR' or another such
;; environment variable in `shell-mode', `term-mode' and `eshell-mode'
;; buffers. Use this Emacs command before executing a shell command which needs
;; the editor set, or always arrange for the current Emacs instance to be used
;; as editor by adding it to the appropriate mode hooks:

;; (add-hook 'shell-mode-hook  'with-editor-export-editor)
;; (add-hook 'term-mode-hook   'with-editor-export-editor)
;; (add-hook 'eshell-mode-hook 'with-editor-export-editor)

;; Some variants of this function exist, these two forms are
;; equivalent:

;; (add-hook 'shell-mode-hook
;;           (apply-partially 'with-editor-export-editor "GIT_EDITOR"))
;; (add-hook 'shell-mode-hook 'with-editor-export-git-editor)

;; This library can also be used by other packages which need to use
;; the current Emacs instance as editor.  In fact this library was
;; written for Magit and its `git-commit-mode' and `git-rebase-mode'.
;; Consult `git-rebase.el' and the related code in `magit-sequence.el'
;; for a simple example.


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
(define-key my-prog-vcs-git-map (kbd "L") 'magit-log-buffer-file)
;; repository log - "L"
;; [M-n/p] to navigate.
;; (define-key my-prog-vcs-git-map (kbd "L") 'magit-log-long)
;; grep - "g"
(define-key my-prog-vcs-git-map (kbd "g") 'magit-grep)
;; checkout - "o"
(define-key my-prog-vcs-git-map (kbd "o") 'magit-checkout) ; magit-checkout-branch-at-point
;; bisect -- "b"
(define-key my-prog-vcs-git-map (kbd "B") 'magit-bisect)
;; blame -- "h"
(define-key my-prog-vcs-git-map (kbd "b") 'magit-blame)
;; file popup
(define-key my-prog-vcs-git-map (kbd "f") 'magit-file-popup)

;; for Magit auto-complete
;; TODO: how to use this in Magit.
;; (setq magit-repository-directories '("~/code/" "~/Git/")
;;       magit-repository-directories-depth 4)

;;; Magit Faces
;;
;; - [M-x customize-group magit-faces]

;; TODO: set remote branch face like this:
;; https://emacs.stackexchange.com/questions/10975/customize-face-magit-item-highlight-properly


;;; auto display magit help popup
;; (defadvice magit-status (after my-magit-status-dispatch-popup)
;;   (magit-dispatch-popup))
;; (ad-activate 'magit-status)

;; enable ispell words complete in commit message buffer.
(add-hook 'git-commit-mode-hook
          '(lambda ()
             (add-to-list (make-local-variable 'company-backends)
                          'company-ispell)))


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


;;; [ magit-gitflow ] -- Git Flow plugin for magit

;;; Usage:
;;
;; - [C-f] in magit status buffer and you will be presented with gitflow popup menu.
;; - All gitflow commands are also accessible through the Magit/Extensions/GitFlow pop-down menu.

(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)


;;; [ git-messenger ] -- popup commit message at current line.

(use-package git-messenger
  :config
  (setq git-messenger:show-detail t ; always show detail message.
        ;; git-messenger:handled-backends '(git svn)
        )
  (define-key my-prog-vcs-map (kbd "m m") 'git-messenger:popup-message)
  (define-key git-messenger-map (kbd "m") 'git-messenger:copy-message)
  (define-key git-messenger-map (kbd "c") 'git-messenger:copy-message)
  ;; enable `magit-commit-mode' after typing 's', 'S', 'd'
  (add-hook 'git-messenger:popup-buffer-hook 'magit-commit-mode)
  )


;;; [ smerge-mode ] -- minor mode to simplify editing output from the diff3 program.

;;; Usage:
;;
;; 1. open conflict file.
;; 2. [M-x smerge-mode] :: highlight all conflict regions, add keybindings.
;;
;; - `smerge-start-session'
;;    Turn on `smerge-mode' and move point to first conflict marker.
;;    If no conflict maker is found, turn off `smerge-mode'.
;;
;; - `smerge-ediff'
;;    - [n/p] :: navigate.
;;    - [a/b] :: accept version?
;;    - [/]   :: look at the ancestor.
;;    - [q]   :: quit the ediff session.

(setq smerge-command-prefix (kbd "C-c v d"))

(define-key smerge-mode-map (kbd "M-g n") 'smerge-next)
(define-key smerge-mode-map (kbd "M-g p") 'smerge-prev)
;; (define-key smerge-mode-map (kbd "M-g k c") 'smerge-keep-current)
;; (define-key smerge-mode-map (kbd "M-g k m") 'smerge-keep-mine)
;; (define-key smerge-mode-map (kbd "M-g k o") 'smerge-keep-other)
;; (define-key smerge-mode-map (kbd "M-g k b") 'smerge-keep-base)
;; (define-key smerge-mode-map (kbd "M-g k a") 'smerge-keep-all)
;; (define-key smerge-mode-map (kbd "M-g e") 'smerge-ediff)
;; (define-key smerge-mode-map (kbd "M-g K") 'smerge-kill-current)
;; (define-key smerge-mode-map (kbd "M-g m") 'smerge-context-menu)
;; (define-key smerge-mode-map (kbd "M-g M") 'smerge-popup-context-menu)

;; enable `smerge-mode' automatically
;; TODO: test current Emacs already support auto enable smerge-mode.
;;
;; (defun my-enable-smerge-mode-auto ()
;;   (when (and buffer-file-name (vc-backend buffer-file-name))
;;     (save-excursion
;;       (goto-char (point-min))
;;       (when (re-search-forward "^<<<<<<< " nil t)
;;         (smerge-mode +1)))))
;; (add-hook 'buffer-list-update-hook #'my-enable-smerge-mode-auto)
;; (add-hook 'find-file-hook #'my-enable-smerge-mode-auto)



;;; [ helm-cmd-t ] -- Helm functions to package directories (SCM controlled or not) as sources.

(use-package helm-cmd-t
  :config
  ;; add-to-list `helm-cmd-t-repo-types'
  (global-set-key (kbd "M-t") 'helm-cmd-t)

  ;; Creating an ad-hoc source
  ;; It’s easy to convert any file system directory into a source
  ;;
  ;; (setq downloads-source (helm-cmd-t-get-create-source-dir "~/Downloads"))
  ;; (setq docs-source (helm-cmd-t-get-create-source-dir "~/Documents"))
  ;;
  ;; (defun helm-cmd-t-ad-hoc-example ()
  ;;   "Choose file from test folder."
  ;;   (interactive)
  ;;   (helm :sources (list downloads-source docs-source)))

  ;; (setq helm-cmd-t-default-repo "~/Git/dotfiles"
  ;;       ;; helm-cmd-t-find-command "find" ; find
  ;;       ;; helm-cmd-t-find-ignored-files
  ;;       )

  )



(require 'init-my-prog-vcs-git-gutter)
(require 'init-my-prog-vcs-github)



(provide 'init-my-prog-vcs-git)

;;; init-my-prog-vcs-git.el ends here
