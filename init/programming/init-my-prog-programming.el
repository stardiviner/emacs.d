;;; init-my-prog-programming.el --- init for common Programming
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Multiple Inferior buffers ]

;;; Shell
(unless (boundp 'my-inferior-shell-map)
  (define-prefix-command 'my-inferior-shell-map))
(define-key inferior-map (kbd "s") 'my-inferior-shell-map)

;;; Lisp
(unless (boundp 'my-inferior-lisp-map)
  (define-prefix-command 'my-inferior-lisp-map))
(define-key inferior-map (kbd "l") 'my-inferior-lisp-map)

;; Emacs Lisp
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
(define-key inferior-map (kbd "r") 'my-inferior-ruby-map)

(define-key my-inferior-ruby-map (kbd "r") 'run-ruby) ; Ruby
;; Python
;; (unless (boundp 'my-inferior-python-map)
;;   (define-prefix-command 'my-inferior-python-map))
;; (define-key inferior-map (kbd "p") 'my-inferior-python-map)

(define-key inferior-map (kbd "p") 'run-python)   ; Python
;; Prolog
(define-key inferior-map (kbd "g") 'run-prolog)   ; Prolog
;; Octave
(define-key inferior-map (kbd "o") 'run-octave)   ; Octave
;; Haskell
(define-key inferior-map (kbd "h") 'run-haskell)  ; Haskell
;; Erlang
(define-key inferior-map (kbd "e") 'run-erlang)   ; Erlang
;; Festival
(define-key inferior-map (kbd "f") 'run-festival) ; Festival


(provide 'init-my-prog-programming)

;;; init-my-prog-programming.el ends here
