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
;; So there are three bindings to remember and they are also documented in
;; mode-line. For more, hitting:
;; - [C-h m]
;; while in helm session will show you all other bindings.
;; NOTE: Some helm commands have a special keymap, you can access infos
;; on these keymap with [C-c ?], it should be documented in the mode-line.
;;
;; Helm prefix key
;; - [C-x c]
;;
;; get helm help in heml minor mode:
;;  - [C-h m]
;;  - M-x helm-mini
;; wildcard match
;;  - ~/_esk (here `_' is a space)

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
;;
(setq helm-completing-read-handlers-alist
      '((describe-function . helm-completing-read-symbols)
        (describe-variable . helm-completing-read-symbols)
        (debug-on-entry . helm-completing-read-symbols)
        (find-function . helm-completing-read-symbols)
        (find-tag . helm-completing-read-with-cands-in-buffer)
        (ffap-alternate-file)
        (find-alternate-file . ido)
        (tmm-menubar)))


;; (global-set-key (kbd "C-x h") 'helm-mini)
(global-set-key (kbd "M-x") 'helm-M-x)
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



(defun my-helm ()
  "My preconfigured `helm'."
  (interactive)
  (condition-case nil
      (if (projectile-project-root)
          (helm-projectile)
        ;; otherwise fallback to `helm-mini'
        (helm-mini))
    ;; fall back to helm mini if an error occurs (usually in `projectile-project-root')
    (error (helm-mini))))

(global-set-key (kbd "C-x h") 'my-helm)




(provide 'init-helm)

;;; init-helm.el ends here
