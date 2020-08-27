;;; init-prog-eval.el --- init for interactive evaluation.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(unless (boundp 'eval-prefix)
  (define-prefix-command 'eval-prefix))
(global-set-key (kbd "C-c e") 'eval-prefix)


;;; [ eval-in-repl ] -- Consistent ESS-like eval interface for various REPLs.

(use-package eval-in-repl
  :ensure t
  :defer t
  :config
  (setq eir-jump-after-eval t)
  (setq eir-delete-other-windows nil
        eir-repl-placement 'right
        eir-always-split-script-window t)

  ;; Emacs Lisp
  (with-eval-after-load 'ielm
    (require 'eval-in-repl-ielm)
    ;; for *.el
    (define-key emacs-lisp-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
    ;; for *scratch*
    (define-key lisp-interaction-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
    ;; for Info Pages
    (define-key Info-mode-map (kbd "<C-return>") 'eir-eval-in-ielm))

  ;; Common Lisp
  ;; SLIME support
  ;; (require 'slime) ; if not done elsewhere
  (with-eval-after-load 'slime
    (require 'eval-in-repl-slime)
    (add-hook 'lisp-mode-hook
              '(lambda ()
                 (local-set-key (kbd "<C-return>") 'eir-eval-in-slime))))

  ;; Geiser support (for Racket and Guile Scheme)
  ;; When using this, turn off racket-mode and scheme supports
  (with-eval-after-load 'geiser
    (require 'eval-in-repl-geiser)
    (add-hook 'geiser-mode-hook
              '(lambda ()
                 (local-set-key (kbd "<C-return>") 'eir-eval-in-geiser))))

  ;; Clojure
  ;; cider support
  (with-eval-after-load 'cider
    (require 'eval-in-repl-cider)
    (define-key clojure-mode-map (kbd "<C-return>") 'eir-eval-in-cider))

  ;; racket-mode support (for Racket; if not using Geiser)
  ;; (require 'racket-mode) ; if not done elsewhere
  ;; (require 'eval-in-repl-racket)
  ;; (define-key racket-mode-map (kbd "<C-return>") 'eir-eval-in-racket)

  ;; Scheme support (if not using Geiser))
  ;; (require 'scheme)    ; if not done elsewhere
  ;; (require 'cmuscheme) ; if not done elsewhere
  ;; (require 'eval-in-repl-scheme)
  ;; (add-hook 'scheme-mode-hook
  ;;    '(lambda ()
  ;;       (local-set-key (kbd "<C-return>") 'eir-eval-in-scheme)))

  ;; Hy support
  ;; (require 'hy-mode) ; if not done elsewhere
  ;; (require 'eval-in-repl-hy)
  ;; (define-key hy-mode-map (kbd "<C-return>") 'eir-eval-in-hy)

  ;; Python support
  (with-eval-after-load 'python
    (require 'eval-in-repl-python)
    (define-key python-mode-map (kbd "<C-return>") 'eir-eval-in-python))

  ;; Ruby support
  ;; (require 'ruby-mode) ; if not done elsewhere
  ;; (require 'inf-ruby)  ; if not done elsewhere
  (with-eval-after-load 'ruby-mode
    (require 'eval-in-repl-ruby)
    (define-key ruby-mode-map (kbd "<C-return>") 'eir-eval-in-ruby))
  (with-eval-after-load 'enh-ruby-mode
    (define-key enh-ruby-mode-map (kbd "<C-return>") 'eir-eval-in-ruby))

  ;; Prolog support
  (with-eval-after-load 'prolog-mode
    (require 'eval-in-repl-prolog)
    (define-key prolog-mode-map (kbd "<C-return>") 'eir-eval-in-prolog))

  ;; SML support
  ;; (require 'sml-mode) ; if not done elsewhere
  ;; (require 'eval-in-repl-sml)
  ;; (define-key sml-mode-map (kbd "<C-return>") 'eir-eval-in-sml)
  ;; (define-key sml-mode-map (kbd "C-;") 'eir-send-to-sml-semicolon)

  ;; OCaml support
  ;; (require 'tuareg) ; if not done elsewhere
  ;; (require 'eval-in-repl-ocaml)
  ;; (define-key tuareg-mode-map (kbd "<C-return>") 'eir-eval-in-ocaml)
  ;; ;; function to send a semicolon to OCaml REPL
  ;; (define-key tuareg-mode-map (kbd "C-;") 'eir-send-to-ocaml-semicolon)

  ;; Shell support
  (require 'eval-in-repl-shell)
  (add-hook 'sh-mode-hook
            '(lambda () (define-key sh-mode-map (kbd "<C-return>") 'eir-eval-in-shell)))

  ;; JavaScript support
  (require 'eval-in-repl-javascript)
  (define-key js2-mode-map (kbd "<C-return>") 'eir-eval-in-javascript))

;;; [ evalator ] -- Package for interactive transformation of data with helm.

;; (use-package evalator
;;   :ensure t
;;   :ensure evalator-clojure
;;   :defer t
;;   :init
;;   (unless (boundp 'evalator-prefix)
;;     (define-prefix-command 'evalator-prefix))
;;   (define-key eval-prefix (kbd "v") 'evalator-prefix)
;;   (define-key evalator-prefix (kbd "e") 'evalator)
;;   (define-key evalator-prefix (kbd "x") 'evalator-explicit)
;;   (define-key evalator-prefix (kbd "r") 'evalator-resume)
;;   (define-key evalator-prefix (kbd "i") 'evalator-insert-equiv-expr)
;;   :config
;;   ;; auto detect context
;;   (setq evalator-config-mode-context-alist nil)
;;   (add-to-list 'evalator-config-mode-context-alist
;;                '(ruby-mode . evalator-ruby-context))
;;   (add-to-list 'evalator-config-mode-context-alist
;;                '(clojure-mode . evalator-clojure-context)))

;;; [ play-code ] -- Play code with online playgrounds.

;; (use-package playonline
;;   :ensure t
;;   :commands (playonline)
;;   :bind (:map eval-prefix ("o" . playonline))
;;   :init (add-to-list 'display-buffer-alist '("^\\*playonline\\*" . (display-buffer-below-selected)))
;;   (use-package ob-playonline
;;     :quelpa (ob-playonline :fetcher github :repo "twlz0ne/ob-playonline")
;;     :demand t))

;;; [ sesman ] -- Session manager for Emacs based IDEs.

(use-package sesman
  :ensure t
  :commands (sesman-start sesman-restart sesman-quit))


(provide 'init-prog-eval)

;;; init-prog-eval.el ends here
