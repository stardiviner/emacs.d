;;; init-my-prog-programming.el --- init for common Programming
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Multiple Inferior buffers ]

;; Emacs Lisp
(define-key inferior-map (kbd "e") 'my-ielm-start-or-switch)
;; Lisp dialects
(define-key inferior-map (kbd "l l") 'run-geiser) ; Common Lisp
(define-key inferior-map (kbd "l c") 'run-lisp)   ; Clojure cider.
(define-key inferior-map (kbd "l s") 'run-scheme) ; Scheme
(define-key inferior-map (kbd "l g") 'run-guile)  ; Guile
(define-key inferior-map (kbd "l m") 'slime)      ; SLIME
;; Ruby
(define-key inferior-map (kbd "r") 'run-ruby)     ; Ruby
;; Python
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
