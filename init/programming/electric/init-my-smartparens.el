;;; init-my-smartparens.el --- init smartparens
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ show-paren-mode ]

(show-paren-mode 1) ; highlight matched parentheses

(setq show-paren-style 'parenthesis)

(set-face-attribute 'show-paren-match-face nil
                    :foreground "white" :background "deep pink"
                    :weight 'bold
                    )
(set-face-attribute 'show-paren-mismatch-face nil
                    :background "brown"
                    :strike-through t
                    )



;;; [ smartparens ] -- Minor mode for Emacs that deals with parens pairs and tries to be smart about it.

;;;_* Usage:
;; https://github.com/Fuco1/smartparens/wiki
;; https://github.com/Fuco1/smartparens/wiki#what-is-this-package-about?

;;; add pairs
;; (sp-with-modes '(rhtml-mode)
;;   ;; (sp-local-pair "<" ">")
;;   (sp-local-pair "<%" "%>"))
;;
;;; disable pair
;; (sp-local-pair '(web-mode)
;;                     "<" nil
;;                     :actions '(:rem insert))

(use-package smartparens
  :ensure t
  :config
  ;; require
  (require 'smartparens-config)

  ;; enable smartparens-mode
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)


  ;; options
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
        ;; sp-paredit-bindings '(("C-M-f" . sp-forward-sexp)
        ;;                       ("C-M-b" . sp-backward-sexp)
        ;;                       ("C-M-u" . sp-backward-up-sexp)
        ;;                       ("C-M-d" . sp-down-sexp)
        ;;                       ("C-M-p" . sp-backward-down-sexp)
        ;;                       ("C-M-n" . sp-up-sexp)
        ;;                       ("M-s" . sp-splice-sexp)
        ;;                       ("M-<up>" . sp-splice-sexp-killing-backward)
        ;;                       ("M-<down>" . sp-splice-sexp-killing-forward)
        ;;                       ("M-r" . sp-splice-sexp-killing-around)
        ;;                       ("C-)" . sp-forward-slurp-sexp)
        ;;                       ("C-<right>" . sp-forward-slurp-sexp)
        ;;                       ("C-}" . sp-forward-barf-sexp)
        ;;                       ("C-<left>" . sp-forward-barf-sexp)
        ;;                       ("C-(" . sp-backward-slurp-sexp)
        ;;                       ("C-M-<left>" . sp-backward-slurp-sexp)
        ;;                       ("C-{" . sp-backward-barf-sexp)
        ;;                       ("C-M-<right>" . sp-backward-barf-sexp)
        ;;                       ("M-S" . sp-split-sexp))
        ;;
        ;; sp-smartparens-bindings '(("C-M-f" . sp-forward-sexp)
        ;;                           ("C-M-b" . sp-backward-sexp)
        ;;                           ("C-M-d" . sp-down-sexp)
        ;;                           ("C-M-a" . sp-backward-down-sexp)
        ;;                           ("C-S-d" . sp-beginning-of-sexp)
        ;;                           ("C-S-a" . sp-end-of-sexp)
        ;;                           ("C-M-e" . sp-up-sexp)
        ;;                           ("C-M-u" . sp-backward-up-sexp)
        ;;                           ("C-M-n" . sp-next-sexp)
        ;;                           ("C-M-p" . sp-previous-sexp)
        ;;                           ("C-M-k" . sp-kill-sexp)
        ;;                           ("C-M-w" . sp-copy-sexp)
        ;;                           ("M-<delete>" . sp-unwrap-sexp)
        ;;                           ("M-<backspace>" . sp-backward-unwrap-sexp)
        ;;                           ("C-<right>" . sp-forward-slurp-sexp)
        ;;                           ("C-<left>" . sp-forward-barf-sexp)
        ;;                           ("C-M-<left>" . sp-backward-slurp-sexp)
        ;;                           ("C-M-<right>" . sp-backward-barf-sexp)
        ;;                           ("M-D" . sp-splice-sexp)
        ;;                           ("C-M-<delete>" . sp-splice-sexp-killing-forward)
        ;;                           ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
        ;;                           ("C-S-<backspace>" . sp-splice-sexp-killing-around)
        ;;                           ("C-]" . sp-select-next-thing-exchange)
        ;;                           ("C-M-]" . sp-select-next-thing)
        ;;                           ("M-F" . sp-forward-symbol)
        ;;                           ("M-B" . sp-backward-symbol))
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
  (dolist (hook
           '(clojure-mode-hook
             cider-repl-mode-hook
             emacs-lisp-mode-hook
             eval-expression-minibuffer-setup-hook
             ielm-mode-hook ; inferior-emacs-lisp-mode-hook
             lisp-mode-hook
             lisp-interaction-mode-hook
             scheme-mode-hook
             ))
    (add-hook hook 'turn-on-smartparens-strict-mode))

  ;; TODO: add paredit like keybindings with smartparens-mode.
  ;; add paredit like keybindings with smartparens-mode.
  ;; (define-key smartparens-strict-mode-map (kbd "C-M-b") 'sp-backward-sexp)

  ;; keybinding management
  ;;
  ;; (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
  ;; (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)
  ;;
  ;; (define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
  ;; (define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)
  ;; (define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
  ;; (define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)
  ;;
  ;; (define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
  ;; (define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
  ;; (define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
  ;; (define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)
  ;;
  ;; (define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
  ;; (define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)
  ;;
  ;; (define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
  ;; (define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)
  ;;
  ;; (define-key sp-keymap (kbd "M-<delete>") 'sp-unwrap-sexp)
  ;; (define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)
  ;;
  ;; (define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
  ;; (define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
  ;; (define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
  ;; (define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)
  ;;
  ;; (define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
  ;; (define-key sp-keymap (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
  ;; (define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
  ;; (define-key sp-keymap (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)
  ;;
  ;; (define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
  ;; (define-key sp-keymap (kbd "C-<left_bracket>") 'sp-select-previous-thing)
  ;; (define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)
  ;;
  ;; (define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
  ;; (define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)
  ;;
  ;; (define-key sp-keymap (kbd "H-t") 'sp-prefix-tag-object)
  ;; (define-key sp-keymap (kbd "H-p") 'sp-prefix-pair-object)
  ;; (define-key sp-keymap (kbd "H-s c") 'sp-convolute-sexp)
  ;; (define-key sp-keymap (kbd "H-s a") 'sp-absorb-sexp)
  ;; (define-key sp-keymap (kbd "H-s e") 'sp-emit-sexp)
  ;; (define-key sp-keymap (kbd "H-s p") 'sp-add-to-previous-sexp)
  ;; (define-key sp-keymap (kbd "H-s n") 'sp-add-to-next-sexp)
  ;; (define-key sp-keymap (kbd "H-s j") 'sp-join-sexp)
  ;; (define-key sp-keymap (kbd "H-s s") 'sp-split-sexp)



  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

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


;;;_ provide init
(provide 'init-my-smartparens)

;;; init-my-smartparens.el ends here
