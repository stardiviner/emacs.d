;;; init-prog-electric.el --- init electric stuff.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ electric ]

;; `electric-quote-replace-double'

;;; [ show-paren-mode ]

;; (use-package paren
;;   :config
;;   (show-paren-mode 1) ; highlight matched parentheses
;;   (setq show-paren-style 'parenthesis)
;;   )

;;; [ smartparens ] -- deals with parens pairs and tries to be smart about it.

(use-package smartparens
  :ensure t
  :defer t
  :load (smartparens-config)
  :init (smartparens-global-mode t)
  ;; (show-smartparens-global-mode t)
  :config
  (add-to-list 'sp-ignore-modes-list 'org-mode)
  (setq sp-navigate-consider-sgml-tags '(html-erb-mode
                                         web-mode
                                         nxml-mode sgml-mode
                                         nxhtml-mode html-mode rhtml-mode
                                         jinja2-mode)
        ;; sp-ignore-modes-list '(minibuffer-inactive-mode)
        ;; sp-override-key-bindings '((\"C-M-f\" . sp-forward-sexp)
        ;;                            (\"C-<right>\" . nil))
        )

  (sp-local-pair 'minibuffer-inactive-mode
                 "'" nil :actions nil)

  ;; `code` in clojure comment
  (sp-with-modes '(clojure-mode)
    (sp-local-pair "`" "`"))
  
  ;; smartparens for other modes.
  (require 'smartparens-ruby)

  (require 'smartparens-html)
  ;; (sp-with-modes '(html-mode sgml-mode)
  ;;   (sp-local-pair "<" ">"))
  (sp-with-modes '(rhtml-mode)
    ;; (sp-local-pair "<" ">")
    (sp-local-pair "<%" "%>"))

  (sp-with-modes '(markdown-mode gfm-mode rst-mode)
    (sp-local-pair "*" "*" :bind "C-*")
    (sp-local-tag "2" "**" "**")
    (sp-local-tag "s" "```scheme" "```")
    (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))
  )


(provide 'init-prog-electric)

;;; init-prog-electric.el ends here
