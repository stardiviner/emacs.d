;;; init-my-prog-programming.el --- init for common Programming
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Multiple Inferior buffers ]

(unless (boundp 'my-prog-inferior-prefix-map)
    (define-prefix-command 'my-prog-inferior-prefix-map))
(global-set-key (kbd "C-c i") 'my-prog-inferior-prefix-map)

;; Emacs Lisp
(define-key my-prog-inferior-prefix-map (kbd "e") 'my-ielm-start-or-switch)
;; Lisp dialects
(define-key my-prog-inferior-prefix-map (kbd "l l") 'run-geiser) ; Common Lisp
(define-key my-prog-inferior-prefix-map (kbd "l c") 'run-lisp) ; Clojure cider.
(define-key my-prog-inferior-prefix-map (kbd "l s") 'run-scheme) ; Scheme
(define-key my-prog-inferior-prefix-map (kbd "l g") 'run-guile) ; Guile
;; Ruby
(define-key my-prog-inferior-prefix-map (kbd "r") 'run-ruby) ; Ruby
;; Python
(define-key my-prog-inferior-prefix-map (kbd "p") 'run-python) ; Python
;; Prolog
(define-key my-prog-inferior-prefix-map (kbd "g") 'run-prolog) ; Prolog
;; Octave
(define-key my-prog-inferior-prefix-map (kbd "o") 'run-octave) ; Octave
;; Haskell
(define-key my-prog-inferior-prefix-map (kbd "h") 'run-haskell) ; Haskell
;; Erlang
(define-key my-prog-inferior-prefix-map (kbd "e") 'run-erlang) ; Erlang
;; Festival
(define-key my-prog-inferior-prefix-map (kbd "f") 'run-festival) ; Festival


(provide 'init-my-prog-programming)

;;; init-my-prog-programming.el ends here
