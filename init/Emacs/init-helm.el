;;; init-helm.el --- init Helm
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Helm ] --- (incremental completion and selection narrowing framework)

;; Customize:
;; - [C-c c] -- for all complete framework prefix.

;; Basic usage:
;;  - M-x helm-mini
;; general helm commands:
;;  - [TAB] -- access to `action' menu with
;;  - [C-z] -- use persistent actions with
;;  - [M-SPACE] -- mark candidate with
;; get helm help in heml minor mode:
;;  - [C-h m]
;; wildcard match
;;  - ~/_esk (here `_' is a space)

(require 'helm)
(require 'helm-config)

(require 'helm-misc)

(helm-mode 1)
(diminish 'helm-mode)

(set-face-attribute 'helm-selection nil
                    :background "#004A5D" :foreground "white"
                    :box '(:color "cyan" :line-width 1)
                    :underline nil)

;; (global-set-key (kbd "C-x h") 'helm-mini)

(setq helm-case-fold-search t
      helm-full-frame nil ; use current window as popup.
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
