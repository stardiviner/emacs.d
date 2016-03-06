;;; init-my-prog-inferior.el --- init for Inferior
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(unless (boundp 'my-prog-inferior-map)
  (define-prefix-command 'my-prog-inferior-map))
(global-set-key (kbd "C-c i") 'my-prog-inferior-map)

;;; Shell
(unless (boundp 'my-inferior-shell-map)
  (define-prefix-command 'my-inferior-shell-map))
(define-key my-prog-inferior-map (kbd "s") 'my-inferior-shell-map)

(define-key my-inferior-shell-map (kbd "s") 'shell)

;;; Lisp
(unless (boundp 'my-inferior-lisp-map)
  (define-prefix-command 'my-inferior-lisp-map))
(define-key my-prog-inferior-map (kbd "l") 'my-inferior-lisp-map)

;; Emacs Lisp
(defun my-ielm-start-or-switch ()
  "Start IELM or switch to its buffer if it already exist."
  (interactive)
  (let ((default-directory (getenv "HOME")))
    (my-func/open-and-switch-to-buffer 'ielm "*ielm*" t)))

(defun my-run-sly ()
  "Start SLY or switch to its buffer if it already exist."
  (interactive)
  (my-func/open-and-switch-to-buffer 'sly "*sly-mrepl for sbcl*" t))

;; Lisp dialects
(define-key my-inferior-lisp-map (kbd "e") 'my-ielm-start-or-switch)
(define-key my-inferior-lisp-map (kbd "l") 'run-lisp)   ; Lisp: *inferior-lisp*
(define-key my-inferior-lisp-map (kbd "s") 'slime-repl) ; SLIME REPL
(define-key my-inferior-lisp-map (kbd "y") 'my-run-sly) ; SLY
(define-key my-inferior-lisp-map (kbd "S") 'run-scheme) ; Scheme
(define-key my-inferior-lisp-map (kbd "g") 'run-geiser) ; geiser
(define-key my-inferior-lisp-map (kbd "G") 'run-guile)  ; Guile
(define-key my-inferior-lisp-map (kbd "c") 'my-cider-scratch) ; CIDER scratch


;; Ruby
(unless (boundp 'my-inferior-ruby-map)
  (define-prefix-command 'my-inferior-ruby-map))
(define-key my-prog-inferior-map (kbd "r") 'my-inferior-ruby-map)
(define-key my-inferior-ruby-map (kbd "r") 'my-run-ruby) ; `run-ruby'
(defun my-run-ruby ()
  "Start SLY or switch to its buffer if it already exist."
  (interactive)
  (my-func/open-and-switch-to-buffer 'run-ruby "*ruby*" t))

;; Python
;; (unless (boundp 'my-inferior-python-map)
;;   (define-prefix-command 'my-inferior-python-map))
;; (define-key my-prog-inferior-map (kbd "p") 'my-inferior-python-map)
(define-key my-prog-inferior-map (kbd "p") 'run-python)   ; Python
;; Prolog
(define-key my-prog-inferior-map (kbd "g") 'run-prolog)   ; Prolog
;; ESS
(unless (boundp 'my-inferior-ess-map)
  (define-prefix-command 'my-inferior-ess-map))
(define-key my-prog-inferior-map (kbd "E") 'my-inferior-ess-map)

(define-key my-inferior-ess-map (kbd "E") 'inferior-ess) ; ESS
;; R
;; Julia
;; Octave
(define-key my-prog-inferior-map (kbd "o") 'run-octave)   ; Octave
;; JavaScript
(define-key my-prog-inferior-map (kbd "j") 'run-js) ; JavaScript
;; Haskell
(define-key my-prog-inferior-map (kbd "h") 'run-haskell)  ; Haskell
;; Erlang
(define-key my-prog-inferior-map (kbd "e") 'run-erlang)   ; Erlang
;; Festival
(define-key my-prog-inferior-map (kbd "f") 'run-festival) ; Festival


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
