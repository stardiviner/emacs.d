;;; init-my-emacs-help.el --- init Emacs's help settings.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Info ]
;; (setq Info-directory-list "/usr/share/info/"
;;       Info-additional-directory-list)

(setq Info-isearch-search t) ; let `s' search like =isearch= for incremental search.

;; (Info-mode-hook)


;;; [ info+ ]

(require 'info+)

;; TODO:
;;  Faces defined here:
;;
;;    `info-command-ref-item', `info-constant-ref-item', `info-file',
;;    `info-function-ref-item',`info-macro-ref-item', `info-menu',
;;    `info-node', `info-quoted-name', `info-reference-item',
;;    `info-single-quote', `info-special-form-ref-item',
;;    `info-string', `info-syntax-class-item',
;;    `info-user-option-ref-item', `info-variable-ref-item',
;;    `info-xref'.

(setq Info-breadcrumbs-in-header-flag t
      ;; Info-display-node-header-fn
      Info-fit-frame-flag t
      Info-fontify-angle-bracketed-flag t
      Info-fontify-quotations-flag t
      Info-fontify-reference-items-flag t
      Info-fontify-single-quote-flag t
      Info-saved-nodes t
      ;; Info-subtree-separator "
      ;; * "
      )



(setq suggest-key-bindings t) ; show the /equivalent/ key binding when [M-x] command has one.

(define-key global-map (kbd "C-h u") 'manual-entry) ; Unix


;;; [ Discover.el ]

;;; https://github.com/mickeynp/discover.el

;;  Useful Helper Commands
;;
;; You can get the name of the command that reveals a given context menu by
;; calling `discover-get-context-menu-command-name'. If you just want to funcall
;; the returned symbol, the function `discover-show-context-menu' will do this
;; for you.

;; (require 'discover)
;; (global-discover-mode 1)

;; (when (featurep 'discover)
;;   (discover-add-context-menu))




(provide 'init-my-emacs-help)

;;; init-my-emacs-help.el ends here
