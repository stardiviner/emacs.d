;;; init-my-prog-electric.el --- init electric stuff.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


;; (electric-indent-mode 1)

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


;;; [ smartparens ] -- Minor mode for Emacs that deals with parens pairs and tries to be smart about it.

(use-package smartparens
  :ensure t
  :defer t
  :init
  ;; require
  (require 'smartparens-config)
  ;; enable smartparens-mode
  ;; (smartparens-global-mode t)
  ;; (show-smartparens-global-mode t)

  :config
  (setq sp-highlight-wrap-overlay t
        sp-highlight-pair-overlay t
        sp-highlight-wrap-tag-overlay t
        ;; sp-undo-pairs-separately
        sp-navigate-consider-sgml-tags '(html-erb-mode
                                         jinja2-mode
                                         web-mode
                                         nxml-mode sgml-mode
                                         nxhtml-mode rhtml-mode html-mode)
        sp-ignore-modes-list '(minibuffer-inactive-mode
                               ;; Lisp dialects modes which will use paredit.
                               emacs-lisp-mode
                               inferior-emacs-lisp-mode ; -> ielm-mode
                               lisp-mode lisp-interaction-mode
                               scheme-mode
                               clojure-mode cider-repl-mode
                               ;; other modes which built-in already
                               ;; web-mode
                               )
        sp-autowrap-region t
        sp-autoinsert-pair t
        sp-autoinsert-if-followed-by-word t
        sp-show-pair-delay 0.125
        ;; sp-show-enclosing-pair-commands '(sp-show-enclosing-pair
        ;;                                   sp-forward-slurp-sexp sp-backward-slurp-sexp
        ;;                                   sp-forward-barf-sexp sp-backward-barf-sexp)
        sp-navigate-close-if-unbalanced nil

        ;; sp-pair-list '(("\\\\(" . "\\\\)")
        ;;                ("\\\"" . "\\\"")
        ;;                ("\\(" . "\\)")
        ;;                ("\\{" . "\\}")
        ;;                ("`" . "'")
        ;;                ("{" . "}")
        ;;                ("[" . "]")
        ;;                ("(" . ")")
        ;;                ("\"" . "\""))

        sp-override-key-bindings nil
        sp-autodelete-opening-pair t
        sp-autodelete-closing-pair t
        sp-autodelete-pair t
        sp-message-width 'frame
        ;; sp-show-pair-overlays t
        ;; sp-show-pair-enc-overlays
        )

  ;; set smartparens faces.
  (set-face-attribute 'sp-pair-overlay-face nil
                      :inherit 'highlight
                      )
  (set-face-attribute 'sp-wrap-overlay-face nil
                      :inherit 'sp-pair-overlay-face
                      )
  (set-face-attribute 'sp-wrap-tag-overlay-face nil
                      :inherit 'sp-pair-overlay-face
                      )
  (set-face-attribute 'sp-show-pair-match-face nil
                      :inherit 'show-paren-match-face
                      :foreground "green" :background "black"
                      )
  (set-face-attribute 'sp-show-pair-mismatch-face nil
                      :inherit 'show-paren-mismatch-face
                      :background "brown"
                      )

  ;; use `smartparens-strict-mode' to replace `paredit-mode'.
  ;; (dolist (hook
  ;;          '(clojure-mode-hook
  ;;            cider-repl-mode-hook
  ;;            emacs-lisp-mode-hook
  ;;            ;; eval-expression-minibuffer-setup-hook ; config in minibuffer init file.
  ;;            ielm-mode-hook ; inferior-emacs-lisp-mode-hook
  ;;            lisp-mode-hook
  ;;            lisp-interaction-mode-hook
  ;;            scheme-mode-hook
  ;;            ))
  ;;   (add-hook hook 'turn-on-smartparens-strict-mode))

  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

  ;; `code` in clojure comment
  (sp-with-modes '(clojure-mode)
    (sp-local-pair "`" "`"))
  
  ;; smartparens for other modes.
  (require 'smartparens-ruby)

  ;; (sp-local-pair)
  (sp-with-modes '(rhtml-mode)
    ;; (sp-local-pair "<" ">")
    (sp-local-pair "<%" "%>"))

  ;; html-mode
  ;; (sp-with-modes '(html-mode sgml-mode)
  ;;   (sp-local-pair "<" ">"))
  (require 'smartparens-html)

  ;; markdown-mode
  (sp-with-modes '(markdown-mode gfm-mode rst-mode)
    (sp-local-pair "*" "*" :bind "C-*")
    (sp-local-tag "2" "**" "**")
    (sp-local-tag "s" "```scheme" "```")
    (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))
  )


;;; [ autopair ]

;; (use-package autopair
;;   :ensure t
;;   :defer t
;;   :init
;;   ;; (autopair-global-mode 1)
;;
;;   (dolist (hook
;;            '(prog-mode-hook
;;              ;; ess-mode-hook                ; Emacs Speaks Statistics
;;              ))
;;     (add-hook hook #'(lambda ()
;;                        (unless (and (boundp 'paredit-mode) paredit-mode)
;;                          (autopair-mode))
;;                        )))
;;
;;   :config
;;   (setq autopair-pair-criteria 'help-balance
;;         autopair-skip-criteria 'help-balance
;;         autopair-autowrap 'help-balance ; 'help-balance, t
;;         autopair-blink t)
;;
;;   (setq autopair-extra-pairs `(:everywhere (;; chinese punctuation
;;                                             (?‘. ?’)
;;                                             (?“. ?”)
;;                                             (?（. ?）)
;;                                             (?【. ?】)
;;                                             (?〖. ?〗)
;;                                             (?『. ?』)
;;                                             (?｛. ?｝)
;;                                             (?「. ?」)
;;                                             (?〔. ?〕)
;;                                             (?［. ?］)
;;                                             (?《. ?》)
;;                                             (?〈. ?〉)
;;                                             (?«. ?»)
;;                                             (?‹. ?›)
;;                                             )))
;;
;;
;;   ;; More tricks
;;   ;; prevent the { (opening brace) character from being autopaired in C++ comments.
;;   ;;
;;   ;; (add-hook 'c++-mode-hook
;;   ;;           #'(lambda ()
;;   ;;               (push ?{
;;   ;;                     (getf autopair-dont-pair :comment))))
;;   ;;
;;   ;; autopair-handle-action-fns lets you write some emacs-lisp that
;;   ;; overrides/extends the actions taken by autopair after it decides something
;;   ;; must be paired, skipped or deleted. To work with triple quoting in python
;;   ;; mode, you can use this for example:
;;   ;;
;;   ;; (add-hook 'python-mode-hook
;;   ;;           #'(lambda ()
;;   ;;               (setq autopair-handle-action-fns
;;   ;;                     (list #'autopair-default-handle-action
;;   ;;                           #'autopair-python-triple-quote-action))))
;;   ;;
;;   ;; autopair-extra-pairs lets you define extra pairing and skipping behaviour
;;   ;; for pairs not programmed into the syntax table. Watch out, this is
;;   ;; work-in-progress, a little unstable and does not help balancing at all. To
;;   ;; have < and > pair in c++-mode buffers, but only in code, use:
;;   ;;
;;   ;; (add-hook 'c++-mode-hook
;;   ;;           #'(lambda ()
;;   ;;               (push '(?< . ?>)
;;   ;;                     (getf autopair-extra-pairs :code))))
;;   ;;
;;   ;; if you program in emacs-lisp you might also like the following to pair
;;   ;; backtick (`) and quote (’).
;;   ;;
;;   ;; for quote Emacs Lisp code. e.g. `org-mode'
;;
;;   (add-hook 'emacs-lisp-mode-hook
;;             (lambda ()
;;               (setq-local autopair-extra-pairs `(:comment ((?`. ?'))))
;;               ;; (push '(?` . ?')
;;               ;;       (getf autopair-extra-pairs :comment))
;;               ;; (push '(?` . ?')
;;               ;;       (getf autopair-extra-pairs :string))
;;               )
;;             )
;;
;;   (add-hook 'clojure-mode-hook
;;             (lambda ()
;;               (setq-local autopair-extra-pairs `(:comment ((?`. ?`))))))
;;   )


(provide 'init-my-prog-electric)

;;; init-my-prog-electric.el ends here
