;;; init-my-prog-inferior.el --- init for Inferior
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ comint-mode ]

;; It will always put point back to the statement you entered, right above the
;; output it created.
;; (setq comint-output-filter-functions
;;       (function (lambda (STR) (comint-show-output))))


;;;_ scratch.el -- launch a scratch buffer for the current mode.

(autoload 'scratch "scratch" nil t)

(define-key my-prog-inferior-map (kbd "C-c") 'scratch)



;;; [ eval-in-repl ] -- Consistent ESS-like eval interface for various REPLs.

;;; Usage:
;;
;; - [C-RET] :: rules all!

(use-package eval-in-repl
  :ensure t
  :config
  (setq eir-jump-after-eval t)
  (setq eir-delete-other-windows nil
        eir-repl-placement 'right)

  ;; Emacs Lisp
  (with-eval-after-load 'ielm
    (require 'eval-in-repl-ielm)
    ;; for *.el
    (define-key emacs-lisp-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
    ;; for *scratch*
    (define-key lisp-interaction-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
    ;; for Info Pages
    (define-key Info-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
    )

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
    (define-key clojure-mode-map (kbd "<C-return>") 'eir-eval-in-cider)
    )

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
            '(lambda ()
               (define-key sh-mode-map (kbd "<C-return>") 'eir-eval-in-shell)))

  ;; JavaScript support
  (with-eval-after-load 'js3-mode
    (require 'eval-in-repl-javascript)
    (define-key js3-mode-map (kbd "<C-return>") 'eir-eval-in-javascript))
  (with-eval-after-load 'js2-mode
    (require 'eval-in-repl-javascript)
    (define-key js2-mode-map (kbd "<C-return>") 'eir-eval-in-javascript))
  )


(provide 'init-my-prog-inferior)

;;; init-my-prog-inferior.el ends here
