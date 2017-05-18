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

(define-key my-inferior-lisp-map (kbd "e") 'my-ielm-start-or-switch)

(defun my-scratch-start-or-switch ()
  "Start IELM or switch to its buffer if it already exist."
  (interactive)
  ;; (switch-to-buffer "*scratch*")
  (popwin:display-buffer "*scratch*")
  )

(define-key my-inferior-lisp-map (kbd "k") 'my-scratch-start-or-switch)

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


(provide 'init-my-prog-inferior)

;;; init-my-prog-inferior.el ends here
