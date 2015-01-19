;;; init-my-prog-programming.el --- init for common Programming
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Multiple Inferior buffers ]

;;; Shell
(unless (boundp 'my-inferior-shell-map)
  (define-prefix-command 'my-inferior-shell-map))
(define-key my-prog-inferior-map (kbd "s") 'my-inferior-shell-map)

;;; Lisp
(unless (boundp 'my-inferior-lisp-map)
  (define-prefix-command 'my-inferior-lisp-map))
(define-key my-prog-inferior-map (kbd "l") 'my-inferior-lisp-map)

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

(define-key my-inferior-lisp-map (kbd "e") 'my-ielm-start-or-switch)
;; Lisp dialects
(define-key my-inferior-lisp-map (kbd "l") 'run-geiser) ; Common Lisp
(define-key my-inferior-lisp-map (kbd "c") 'run-lisp)   ; Clojure cider.
(define-key my-inferior-lisp-map (kbd "s") 'run-scheme) ; Scheme
(define-key my-inferior-lisp-map (kbd "g") 'run-guile)  ; Guile
(define-key my-inferior-lisp-map (kbd "m") 'slime)      ; SLIME
;; Ruby
(unless (boundp 'my-inferior-ruby-map)
  (define-prefix-command 'my-inferior-ruby-map))
(define-key my-prog-inferior-map (kbd "r") 'my-inferior-ruby-map)

(define-key my-inferior-ruby-map (kbd "r") 'run-ruby) ; Ruby
;; Python
;; (unless (boundp 'my-inferior-python-map)
;;   (define-prefix-command 'my-inferior-python-map))
;; (define-key my-prog-inferior-map (kbd "p") 'my-inferior-python-map)

(define-key my-prog-inferior-map (kbd "p") 'run-python)   ; Python
;; Prolog
(define-key my-prog-inferior-map (kbd "g") 'run-prolog)   ; Prolog
;; ESS
;; Julia
;; R
(unless (boundp 'my-inferior-ess-map)
  (define-prefix-command 'my-inferior-ess-map))
(define-key my-prog-inferior-map (kbd "j") 'my-inferior-ess-map)
;; Octave
(define-key my-prog-inferior-map (kbd "o") 'run-octave)   ; Octave
;; Haskell
(define-key my-prog-inferior-map (kbd "h") 'run-haskell)  ; Haskell
;; Erlang
(define-key my-prog-inferior-map (kbd "e") 'run-erlang)   ; Erlang
;; Festival
(define-key my-prog-inferior-map (kbd "f") 'run-festival) ; Festival


(provide 'init-my-prog-programming)

;;; init-my-prog-programming.el ends here
