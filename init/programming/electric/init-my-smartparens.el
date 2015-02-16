;;; init-my-smartparens.el --- init smartparens
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ show-paren-mode ]

(show-paren-mode 1) ; highlight matched parentheses

(setq show-paren-style 'mixed) ; 'parenthesis, 'expression, 'mixed

(set-face-attribute 'show-paren-match-face nil
                    :foreground nil :background "black"
                    )
(set-face-attribute 'show-paren-mismatch-face nil
                    :foreground nil :background "brown"
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

;;;_* require
(require 'smartparens-config)

;;;_* options
(setq sp-highlight-wrap-overlay t
      ;; sp-undo-pairs-separately
      sp-navigate-consider-sgml-tags '(html-erb-mode
                                       jinja2-mode
                                       web-mode
                                       nxml-mode nxhtml-mode rhtml-mode sgml-mode html-mode)
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
      sp-highlight-pair-overlay t
      ;; sp-show-pair-overlays t
      ;; sp-show-pair-enc-overlays
      )

;;;_* enable smartparens-mode
(smartparens-global-mode t)
(show-smartparens-global-mode t)

;;;_* set smartparens faces.
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
                    )
(set-face-attribute 'sp-show-pair-mismatch-face nil
                    :inherit 'show-paren-mismatch-face
                    )

;;;_* use `smartparens-strict-mode' to replace `paredit-mode'.
;; (dolist (hook
;;          '(emacs-lisp-mode-hook
;;            eval-expression-minibuffer-setup-hook
;;            ielm-mode-hook ; inferior-emacs-lisp-mode-hook
;;            lisp-mode-hook
;;            lisp-interaction-mode-hook
;;            scheme-mode-hook
;;            clojure-mode-hook
;;            cider-repl-mode-hook
;;            ))
;;   (add-hook hook 'smartparens-strict-mode))

;;;_ + TODO: add paredit like keybindings with smartparens-mode.
;; (define-key smartparens-strict-mode-map (kbd "C-M-b") 'sp-backward-sexp)

;;;_* smartparens for other modes.
(require 'smartparens-ruby)

;; (sp-local-pair)
(sp-with-modes '(rhtml-mode)
  ;; (sp-local-pair "<" ">")
  (sp-local-pair "<%" "%>"))

(require 'smartparens-html)



;;;_ provide init
(provide 'init-my-smartparens)

;;; init-my-smartparens.el ends here
