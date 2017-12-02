;;; init-my-emacs-help.el --- init Emacs's help settings.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;; (setq text-quoting-style nil)

;;; [ Info ]

;; (require 'info)

(add-to-list 'Info-directory-list
             (expand-file-name "info/" user-emacs-directory))
;; - `Info-default-directory-list'

;; let `[s]' search like =isearch= for incremental search.
(setq Info-isearch-search t)

;; - `Info-mode-hook'


;;; [ info+ ]

(use-package info+
  :ensure t
  :config
  (setq Info-breadcrumbs-in-header-flag t
        ;; Info-display-node-header-fn
        Info-fit-frame-flag t
        Info-fontify-angle-bracketed-flag t
        Info-fontify-quotations-flag t
        Info-fontify-reference-items-flag t
        Info-fontify-single-quote-flag t
        Info-saved-nodes t
        )
  )


(provide 'init-my-emacs-help)

;;; init-my-emacs-help.el ends here
