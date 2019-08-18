;;; init-prog-electric.el --- init electric stuff.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ electric ]

;; `electric-quote-replace-double'

;;; [ show-paren-mode ] -- highlight matched parentheses.

;; (use-package paren
;;   :init (show-paren-mode 1)
;;   :config (setq show-paren-style 'parenthesis)
;;   (set-face-attribute 'show-paren-match nil
;;                       :background "green yellow"))

;;; [ smartparens ] -- deals with parens pairs and tries to be smart about it.

;; (use-package smartparens
;;   :ensure t
;;   :defer t
;;   :delight smartparens-mode
;;   :commands (smartparens-mode)
;;   :config
;;   ;; `smartparens' is heavy in `org-self-insert-command'.
;;   (add-to-list 'sp-ignore-modes-list 'org-mode)
;;   (add-to-list 'sp-ignore-modes-list 'emacs-lisp-mode)
;;   (add-to-list 'sp-ignore-modes-list 'clojure-mode)
;;   (add-to-list 'sp-ignore-modes-list 'lisp-mode)
;;   (add-to-list 'sp-ignore-modes-list 'scheme-mode)
;;   (add-to-list 'sp-ignore-modes-list 'python-mode)
;;   ;; (setq sp-navigate-consider-sgml-tags '(html-erb-mode
;;   ;;                                        web-mode
;;   ;;                                        nxml-mode sgml-mode
;;   ;;                                        nxhtml-mode html-mode rhtml-mode
;;   ;;                                        jinja2-mode)
;;   ;;       )
;;   ;; (setq sp-override-key-bindings '((\"C-M-f\" . sp-forward-sexp)
;;   ;;                                  (\"C-<right>\" . nil)))
;;
;;   (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
;;
;;   ;; `code` in clojure comment
;;   (sp-with-modes '(clojure-mode) (sp-local-pair "`" "`"))
;;
;;   ;; smartparens for other modes.
;;   (require 'smartparens-ruby)
;;   (require 'smartparens-html)
;;
;;   (sp-with-modes '(rhtml-mode)
;;     ;; (sp-local-pair "<" ">")
;;     (sp-local-pair "<%" "%>"))
;;
;;   (sp-with-modes '(markdown-mode gfm-mode rst-mode)
;;     (sp-local-pair "*" "*" :bind "C-*")
;;     (sp-local-tag "2" "**" "**")
;;     (sp-local-tag "s" "```scheme" "```")
;;     (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))
;;
;;   ;; (set-face-attribute 'sp-show-pair-match-face nil
;;   ;;                     :background "green yellow")
;;
;;   ;; (smartparens-global-mode t)
;;   ;; (show-smartparens-global-mode t)
;;   (add-hook 'prog-mode-hook #'turn-on-smartparens-mode)
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


(provide 'init-prog-electric)

;;; init-prog-electric.el ends here
