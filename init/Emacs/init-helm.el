;;; init-helm.el --- init Helm
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Helm ] --- (incremental completion and selection narrowing framework)

;;; Usage:
;;;
;;; Helm needs you to remember only a few binding unlike all other Emacs
;;; applications. Thus, all bindings are auto documented.
;;
;; So when helm starts, you have to remember:
;; - [TAB] -- Access to action menu with.
;; - [C-z] -- Use persistent actions with
;; - [M-<space>] -- Mark candidate with
;;
;; Helm prefix key
;; - [C-x c] :: for "helm version" of other functions.
;;
;; So there are three bindings to remember and they are also documented in
;; mode-line. For more, hitting:
;; - [C-h m]
;; while in helm session will show you all other bindings.
;; NOTE: Some helm commands have a special keymap, you can access infos
;; on these keymap with [C-c ?], it should be documented in the mode-line.
;;
;; get helm help in heml minor mode:
;;  - [C-h m]
;;  - M-x helm-mini
;; wildcard match
;;  - ~/_esk (here `_' is a space)
;;
;; [C-x C-f] -- (helm-find-files)
;;
;; Find Files or url: ~/
;; That show all ~/ directory.
;;
;; Find Files or url: ~/des
;; will show all what begin with "des"
;;
;; Find Files or url: ~/ esk
;; (Notice the space after ~/) will show all what contain esk.
;;
;; Find Files or url: ~/ el$
;; Will show all what finish with el
;;
;; When a row is selected, C-z performs the default action, which is different depending on the context.
;; When you are on a file, C-z will show only this file-name in the helm
;; buffer. On a directory, C-z will step down into this directory to continue
;; searching in it. On a symlink, C-z will expand to the true name of symlink
;; (moving your mouse cursor over a symlink will also show the true name).
;;
;; Take advantage of the second, third and 4th actions in helm. Instead of opening action menu with TAB, just hit:
;; C-e for 2th action, C-j for 3th action

(require 'helm)
(require 'helm-config)

(require 'helm-misc)

(helm-mode 1) ; enable Helm mode initially.
(diminish 'helm-mode)

;;; work with ido and helm together.
;; If you like ido for some commands and helm for other commands, you should not
;; enable ido-mode, instead customize helm-completing-read-handlers-alist; For
;; example, you want ido-mode for find-file-read-only and helm-mode for
;; find-file: 1) In your config you turn on helm-mode. 2) In customize-group
;; helm-mode add to helm-completing-read-handlers-alist find-file-read-only as
;; key and ido as value. In elisp it looks like this:
;; (find-file-read-only . ido)
;;
;; Now you want find-alternate-file to not use ido and to not use helm, only the
;; vanilla emacs completion: Add an entry to helm-completing-read-handlers-alist
;; like this: (find-alternate-file . nil)

(setq helm-completing-read-handlers-alist
      '((describe-function . helm-completing-read-symbols)
        (describe-variable . helm-completing-read-symbols)
        (debug-on-entry . helm-completing-read-symbols)
        (find-function . helm-completing-read-symbols)
        (find-tag . helm-completing-read-with-cands-in-buffer)
        (ffap-alternate-file)
        (tmm-menubar)
        ;; customize
        (org-insert-link . ido)         ; NOTE: temp solution for Helm org-insert-link error.
        ))


;; (global-set-key (kbd "C-x h") 'helm-mini)
;; (global-set-key (kbd "M-x") 'helm-M-x) ; conflict with {[C-u M-x align-regexp] on select region text.}
;; If you prefer the helm version of the file finder, you can bind it to C-x C-f
;; to replace the standard find-file:
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(set-face-attribute 'helm-selection nil
                    :background "#004A5D" :foreground "white"
                    :box '(:color "cyan" :line-width 1)
                    :underline nil)
(set-face-attribute 'helm-action nil
                    :background "orange" :foreground "black")
(set-face-attribute 'helm-header nil
                    :reverse-video nil
                    :foreground "deep pink" :background "black"
                    :weight 'bold)
(set-face-attribute 'helm-source-header nil
                    :foreground "white" :background "#22083397778B"
                    :height 1.3 :weight 'bold)
(set-face-attribute 'helm-separator nil
                    :foreground "cyan")
(set-face-attribute 'helm-visible-mark nil
                    :foreground "black" :background "green yellow")

(setq helm-case-fold-search t
      ;; helm popup window position.
      helm-full-frame nil ; use current window as popup.
      helm-always-two-windows t
      helm-split-window-in-side-p t ; force split inside selected window.
      helm-split-window-default-side 'below
      )

;; Bookmark
;; Helm bookmarks [C-x C-x r b]
;; (helm-highlight-bookmark)

;; Firefox bookmarks [C-x C-x]
;; NOTE config your firefox `about:config' to enable:
;; user_pref("browser.bookmarks.autoExportHTML", false);


;; man-women

;;; - helm-man-women ::

;; (setq helm-man-or-woman-function 'Man-getpage-in-background)


;; [ helm-descbinds ]

;; Usage:
;; - [C-h KEY]
;; - [KEY_PREFIX C-h]

;; - when in helm completion buffer:
;;   - press [RET] to select candidate command to execute.
;;   - press [TAB], you can execute action: "execute", "describe function", "find function".
;;   - press [C-z], selected command is described without quiting.

(require 'helm-descbinds)
(helm-descbinds-mode 1)


;; [ helm-projectile ]

(require 'helm-projectile)



;; (defun my-helm ()
;;   "My preconfigured `helm'."
;;   (interactive)
;;   (condition-case nil
;;       (if (projectile-project-root)
;;           (helm-projectile)
;;         ;; otherwise fallback to `helm-mini'
;;         (helm-mini))
;;     ;; fall back to helm mini if an error occurs (usually in `projectile-project-root')
;;     (error (helm-mini))))
;;
;; (global-set-key (kbd "C-x h") 'my-helm)


;; This provides a single command `helm-helm-commands' which will present a helm
;; buffer containing a list of helm commands and short descriptions. You can
;; press C-z on an item to see a longer description of the command, and RET to
;; execute the command.

(defvar helm-helm-commands-source-buffer "*helm source select*")

(defvar helm-source-helm-commands
  `((name . "Helm commands")
    (candidate-number-limit . 9999)
    (candidates
     . (lambda nil
         (loop for symname in (all-completions "helm-" obarray)
               for sym = (intern symname)
               if (commandp sym) collect
               (cons
                (concat
                 (propertize (format "%s" symname)
                             'face 'font-lock-function-name-face)
                 (propertize (format " %s"
                                     (or (and (documentation sym)
                                              (car (split-string
                                                    (documentation sym) "\n\\|\\.")))
                                         "Not documented"))
                             'face 'font-lock-doc-face))
                sym))))
    (action . (("Execute helm command" .
                (lambda (candidate)
                  (call-interactively candidate)))
               ("Describe command" . describe-command)))
    (persistent-action . describe-command)))

(defun helm-helm-commands nil
  "Select from helm commands to execute."
  (interactive)
  (helm :sources 'helm-source-helm-commands
        :buffer helm-helm-commands-source-buffer))


;;; [ helm-project ]



;;; [ helm-ls-git ]

(require 'helm-ls-git)


;;; [ helm-c-yasnippet ] -- helm source for yasnippet.el

;;; Usage:
;; - `helm-yas-complete' :: List of yasnippet snippets using `helm' interface.
;; - `helm-yas-create-snippet-on-region' :: Create a snippet from region.

(require 'helm-c-yasnippet)

(setq helm-yas-space-match-any-greedy t ; helm pattern space match anyword greedy.
      helm-yas-not-display-dups t
      helm-yas-display-msg-after-complete t
      helm-yas-display-key-on-candidate t
      )

(global-set-key (kbd "C-c & C-c") 'helm-yas-complete) ; integrate helm with yasnippet.


;;; [ helm-rails ]

(require 'helm-rails-loaddefs)

;; TODO: test whether has keybinding set by default.
(define-key global-map (kbd "s-t") 'helm-rails-controllers)
(define-key global-map (kbd "s-y") 'helm-rails-models)
(define-key global-map (kbd "s-u") 'helm-rails-views)
(define-key global-map (kbd "s-o") 'helm-rails-specs)
(define-key global-map (kbd "s-r") 'helm-rails-all)


;;; [ helm-gist ]

;;; Usage:
;; - `helm-for-gist'
;; - Helm defined source: `helm-c-source-gist'

(require 'helm-gist)



(provide 'init-helm)

;;; init-helm.el ends here
