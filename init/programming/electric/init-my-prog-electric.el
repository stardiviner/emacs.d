;;; init-my-prog-electric.el --- init electric stuff.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ show-paren-mode ]

(show-paren-mode 1) ; highlight matched parentheses

(setq show-paren-style 'parenthesis)

(set-face-attribute 'show-paren-match nil
                    :foreground "white" :background "deep pink"
                    :weight 'bold
                    )
(set-face-attribute 'show-paren-mismatch nil
                    :background "brown"
                    :strike-through t
                    )


;;; [ smartparens ] -- deals with parens pairs and tries to be smart about it.

(use-package smartparens
  :ensure t
  :defer t
  :init
  (smartparens-global-mode t)

  ;; use `smartparens-strict-mode' to replace `paredit-mode'.
  ;; (dolist (hook
  ;;          '(emacs-lisp-mode-hook
  ;;            ielm-mode-hook ; inferior-emacs-lisp-mode-hook
  ;;            ;; eval-expression-minibuffer-setup-hook ; minibuffer
  ;;            clojure-mode-hook
  ;;            cider-repl-mode-hook
  ;;            lisp-mode-hook
  ;;            lisp-interaction-mode-hook
  ;;            scheme-mode-hook
  ;;            ))
  ;;   (add-hook hook 'turn-on-smartparens-strict-mode))

  :config
  (require 'smartparens-config)
  
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



(provide 'init-my-prog-electric)

;;; init-my-prog-electric.el ends here
