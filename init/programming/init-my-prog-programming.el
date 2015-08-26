;;; init-my-prog-programming.el --- init for common Programming
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ prog-mode ]

;;; `prog-mode' is a major mode provided by Emacs. Typically, it is not used
;;; directly, instead many programming-related major modes are derived from this
;;; mode.

;; User can add things to `prog-mode-hook', which are executed for all
;; programming modes (that are derived from `prog-mode').
;; One benefit of using this mode is that global minor modes no longer have to
;; maintain a long list of suitable major modes. Instead, they can simply check
;; if a mode is derived from one of the base modes.
;; Other often used base modes include `special-mode' and `text-mode'.

;;; Usage:
;;
;; Some major programming modes is not included in `prog-mode' alist.
;; - check out what major modes which current in "prog-mode" list.
;; --> TODO:
;;
;; You can define a new major mode derived from ‘prog-mode’ using the following:
;;
;; (define-derived-mode alpha-mode prog-mode "Alpha"
;;   "Major mode for editing alpha files."
;;   ...)
;;
;; You can check if the major mode of the current buffer is derived from ‘prog-mode’ using:
;;
;; (derived-mode-p 'prog-mode)
;;
;; A global minor mode that will be enabled for all ‘prog-mode’ modes can be defined using:
;;
;; (define-global-minor-mode my-global-mode my-mode
;;   (lambda ()
;;     (when (derived-mode-p 'prog-mode)
;;       (my-mode 1))))

;;; TODO: add un-included programming modes into `prog-mode' alist variable.


;;; Turn a non prog-mode derived major-mode into a prog-mode derived major-mode.
;;;
;;; You should place this after anything else you add to erlang-mode-hook to make
;;; sure prog-mode-hook gets called before anything else. That way erlang-mode
;;; can clobber any settings in prog-mode that it doesn't like.
;;
;; (add-hook 'ruby-mode-hook
;;           (lambda ()
;;             (unless (derived-mode-p 'prog-mode))
;;             (run-hooks 'prog-mode-hook)))

(dolist (hook '(ruby-mode-hook
                html-mode-hook
                css-mode-hook
                ))
  (add-hook hook (lambda ()
                   (unless (derived-mode-p 'prog-mode))
                   (run-hooks 'prog-mode-hook))))


(defvar lisp-dialects-mode
  '(lisp-mode
    lisp-interaction-mode
    emacs-lisp-mode
    ;; common-lisp-mode
    scheme-mode
    clojure-mode
    cider-repl-mode
    ))


;;; [ Multiple Inferior buffers ]

;;; Shell
(unless (boundp 'my-inferior-shell-map)
  (define-prefix-command 'my-inferior-shell-map)
  (define-key my-prog-inferior-map (kbd "s") 'my-inferior-shell-map))

;;; Lisp
(unless (boundp 'my-inferior-lisp-map)
  (define-prefix-command 'my-inferior-lisp-map)
  (define-key my-prog-inferior-map (kbd "l") 'my-inferior-lisp-map))

;; Emacs Lisp
(define-key my-inferior-lisp-map (kbd "s")
  (lambda ()
    (interactive)
    (if (get-buffer "*scratch*")
        (switch-to-buffer "*scratch*")
      (funcall the-command)
      (bury-buffer)
      (when whether-switch-to-buffer
        (switch-to-buffer "*scratch*")))))

(defun my-ielm-start-or-switch ()
  "Start IELM or switch to its buffer if it already exist."
  (interactive)
  (let ((default-directory (getenv "HOME")))
    (my-func/open-and-switch-to-buffer 'ielm "*ielm*" t)))

(defun my-run-sly ()
  "Start SLY or switch to its buffer if it already exist."
  (interactive)
  (my-func/open-and-switch-to-buffer 'sly "*sly-mrepl for sbcl*" t))

(define-key my-inferior-lisp-map (kbd "e") 'my-ielm-start-or-switch)
;; Lisp dialects
(define-key my-inferior-lisp-map (kbd "l") 'run-lisp)   ; Lisp
(define-key my-inferior-lisp-map (kbd "s") 'my-run-sly) ; SLY
(define-key my-inferior-lisp-map (kbd "g") 'run-geiser) ; geiser
(define-key my-inferior-lisp-map (kbd "m") 'slime)      ; SLIME
(define-key my-inferior-lisp-map (kbd "S") 'run-scheme) ; Scheme
(define-key my-inferior-lisp-map (kbd "G") 'run-guile)  ; Guile
;; FIXME: not `cider-jack-in'
;; (define-key my-inferior-lisp-map (kbd "c") 'cider-jack-in) ; Clojure cider


;; Ruby
(unless (boundp 'my-inferior-ruby-map)
  (define-prefix-command 'my-inferior-ruby-map)
  (define-key my-prog-inferior-map (kbd "r") 'my-inferior-ruby-map))
(define-key my-inferior-ruby-map (kbd "r") 'run-ruby) ; Ruby
;; Python
;; (unless (boundp 'my-inferior-python-map)
;;   (define-prefix-command 'my-inferior-python-map))
;; (define-key my-prog-inferior-map (kbd "p") 'my-inferior-python-map)
(define-key my-prog-inferior-map (kbd "p") 'run-python)   ; Python
;; Prolog
(define-key my-prog-inferior-map (kbd "g") 'run-prolog)   ; Prolog
;; ESS
;; R
(unless (boundp 'my-inferior-ess-map)
  (define-prefix-command 'my-inferior-ess-map)
  (define-key my-prog-inferior-map (kbd "e") 'my-inferior-ess-map))
;; Julia
;; Octave
(define-key my-prog-inferior-map (kbd "o") 'run-octave)   ; Octave
;; JavaScript
(define-key my-prog-inferior-map (kbd "j") 'run-js) ; JavaScript
;; Haskell
(define-key my-prog-inferior-map (kbd "h") 'run-haskell)  ; Haskell
;; Erlang
(define-key my-prog-inferior-map (kbd "E") 'run-erlang)   ; Erlang
;; Festival
(define-key my-prog-inferior-map (kbd "f") 'run-festival) ; Festival


(provide 'init-my-prog-programming)

;;; init-my-prog-programming.el ends here
