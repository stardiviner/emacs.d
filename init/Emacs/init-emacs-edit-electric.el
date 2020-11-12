;;; init-emacs-edit-electric.el --- init for Electric
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ electric-indent-mode ]

;; (electric-indent-mode) ; globally
(add-hook 'prog-mode-hook #'electric-indent-local-mode) ; mode locally

;;; [ electric-quote-mode ]

(add-hook 'prog-mode-hook #'electric-quote-local-mode)

;;; [ electric-pair-mode ]

;; (electric-pair-mode) ; globally
(add-hook 'prog-mode-hook #'electric-pair-local-mode) ; mode locally

(setq electric-pair-preserve-balance t
      electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit ; 'ignore
      electric-pair-skip-self t)

;;; [ electric-layout-mode ]

;;; [ show-paren-mode ] -- highlight matched parentheses.

;; (use-package paren
;;   :custom (show-paren-style 'parenthesis) ; 'expression
;;   :custom-face (show-paren-match ((t (:background "green yellow"))))
;;   :init (show-paren-mode 1))

;;; [ smartparens ] -- deals with parens pairs and tries to be smart about it.

;; (use-package smartparens
;;   :ensure t
;;   :defer t
;;   :delight smartparens-mode
;;   :custom-face (sp-show-pair-match-face ((t (:background "green yellow"))))
;;   :config
;;   (add-to-list 'sp-ignore-modes-list 'org-mode) ;`smartparens' is heavy in `org-self-insert-command'.
;;   (add-to-list 'sp-ignore-modes-list 'emacs-lisp-mode)
;;   (add-to-list 'sp-ignore-modes-list 'clojure-mode)
;;   (add-to-list 'sp-ignore-modes-list 'lisp-mode)
;;   (add-to-list 'sp-ignore-modes-list 'scheme-mode)
;;   (add-to-list 'sp-ignore-modes-list 'python-mode)  
;;
;;   ;; smartparens for other modes.
;;   (require 'smartparens-ruby)
;;   (require 'smartparens-html)
;;
;;   (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
;;   (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
;;
;;   ;; `code` in clojure comment
;;   (sp-with-modes '(clojure-mode) (sp-local-pair "`" "`"))
;;   (sp-local-pair 'clojure-mode "'" nil :actions nil)
;;
;;   (sp-with-modes '(rhtml-mode) (sp-local-pair "<%" "%>"))
;;
;;   (sp-with-modes '(markdown-mode gfm-mode rst-mode)
;;                  (sp-local-pair "*" "*" :bind "C-*")
;;                  (sp-local-tag "2" "**" "**")
;;                  (sp-local-tag "s" "```scheme" "```")
;;                  (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))
;;
;;   ;; NOTE: to make `sp-ignore-modes-list' work, put `smartparens-mode' at the end.
;;   (smartparens-global-mode t)
;;   (show-smartparens-global-mode t)
;;   ;; (add-hook 'prog-mode-hook #'turn-on-smartparens-mode)
;;   ;; (add-hook 'c-mode-common-hook 'smartparens-mode)
;;   )

;;; [ rainbow-identifiers ] -- highlight identifiers according to their names.

;; (use-package rainbow-identifiers
;;   :ensure t
;;   :defer t
;;   :init
;;   ;; (add-hook 'prog-mode-hook #'rainbow-identifiers-mode)
;;   (hook-modes c-dialects-mode
;;     (when (memq major-mode '(c-mode c++-mode objc-mode))
;;       (rainbow-identifiers-mode 1))))



(provide 'init-emacs-edit-electric)

;;; init-emacs-edit-electric.el ends here
