;;; init-my-prog-inferior.el --- init for Inferior
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(unless (boundp 'inferior-prefix)
  (define-prefix-command 'inferior-prefix))
(global-set-key (kbd "C-c i") 'inferior-prefix)

;;; Shell
;; (unless (boundp 'shell-prefix)
;;   (define-prefix-command 'shell-prefix))
;; (define-key inferior-prefix (kbd "s") 'shell-prefix)
;;
;; (define-key shell-prefix (kbd "s") 'shell)

(define-key inferior-prefix (kbd "s") 'shell)

;;; Lisp
(unless (boundp 'lisp-prefix)
  (define-prefix-command 'lisp-prefix))
(define-key inferior-prefix (kbd "l") 'lisp-prefix)

;; Emacs Lisp
(defun my-ielm-start-or-switch ()
  "Start IELM or switch to its buffer if it already exist."
  (interactive)
  (let ((default-directory (getenv "HOME")))
    (my-func/open-and-switch-to-buffer 'ielm "*ielm*" t)))

(define-key lisp-prefix (kbd "e") 'my-ielm-start-or-switch)

(defun my-scratch-start-or-switch ()
  "Start IELM or switch to its buffer if it already exist."
  (interactive)
  ;; (switch-to-buffer "*scratch*")
  (popwin:display-buffer "*scratch*")
  )

(define-key lisp-prefix (kbd "k") 'my-scratch-start-or-switch)

(defun my-run-sly ()
  "Start SLY or switch to its buffer if it already exist."
  (interactive)
  (my-func/open-and-switch-to-buffer 'sly "*sly-mrepl for sbcl*" t))

;; Lisp dialects
(define-key lisp-prefix (kbd "e") 'my-ielm-start-or-switch)
(define-key lisp-prefix (kbd "l") 'run-lisp)   ; Lisp: *inferior-lisp*
(define-key lisp-prefix (kbd "s") 'slime-repl) ; SLIME REPL
(define-key lisp-prefix (kbd "y") 'my-run-sly) ; SLY
(define-key lisp-prefix (kbd "S") 'run-scheme) ; Scheme
(define-key lisp-prefix (kbd "g") 'run-geiser) ; geiser
(define-key lisp-prefix (kbd "G") 'run-guile)  ; Guile

;; Ruby
(unless (boundp 'ruby-prefix)
  (define-prefix-command 'ruby-prefix))
(define-key inferior-prefix (kbd "r") 'ruby-prefix)
(define-key ruby-prefix (kbd "r") 'my-run-ruby) ; `run-ruby'
(defun my-run-ruby ()
  "Start SLY or switch to its buffer if it already exist."
  (interactive)
  (my-func/open-and-switch-to-buffer 'run-ruby "*ruby*" t))

;; Python
;; (unless (boundp 'python-prefix)
;;   (define-prefix-command 'python-prefix))
;; (define-key inferior-prefix (kbd "p") 'python-prefix)
(define-key inferior-prefix (kbd "p") 'run-python)   ; Python
;; Prolog
(define-key inferior-prefix (kbd "g") 'run-prolog)   ; Prolog
;; ESS
;; (unless (boundp 'ess-prefix)
;;   (define-prefix-command 'ess-prefix))
;; (define-key inferior-prefix (kbd "E") 'ess-prefix)
;;
;; (define-key ess-prefix (kbd "E") 'inferior-ess) ; ESS
(define-key inferior-prefix (kbd "E") 'inferior-ess) ; ESS
;; R
;; Julia
;; Octave
(define-key inferior-prefix (kbd "o") 'run-octave)   ; Octave
;; JavaScript
(define-key inferior-prefix (kbd "j") 'run-js) ; JavaScript
;; Haskell
(define-key inferior-prefix (kbd "h") 'run-haskell)  ; Haskell
;; Erlang
(define-key inferior-prefix (kbd "e") 'run-erlang)   ; Erlang
;; Festival
(define-key inferior-prefix (kbd "f") 'run-festival) ; Festival


;;; [ comint-mode ]

;; It will always put point back to the statement you entered, right above the
;; output it created.
;; (setq comint-output-filter-functions
;;       (function (lambda (STR) (comint-show-output))))


;;;_ scratch.el -- launch a scratch buffer for the current mode.

;; (autoload 'scratch "scratch" nil t)
;; (define-key inferior-prefix (kbd "C-c") 'scratch)


(provide 'init-my-prog-inferior)

;;; init-my-prog-inferior.el ends here
