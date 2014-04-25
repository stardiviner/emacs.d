;;; init-my-prog-lang-clojure.el ---
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ clojure-mode ]

(require 'clojure-mode)

(eval-after-load 'clojure-mode
  '(progn
     (defun my-clojure-mode-defaults ()
       (clojure-test-mode +1))

     (setq my-clojure-mode-hook 'my-clojure-mode-defaults)

     (add-hook 'clojure-mode-hook
               (lambda ()
                 (run-hooks 'my-clojure-mode-hook)))))


;;; [ cider ] -- CIDER is a Clojure IDE and REPL for Emacs

;;; CIDER (formerly nrepl.el) is the Clojure IDE and REPL for Emacs, built on
;;; top of nREPL, the Clojure networked REPL server. It's a great alternative to
;;; the now deprecated combination of SLIME + swank-clojure.

;;; Usage:
;;
;; Use M-x run-lisp to open a simple REPL subprocess using Leiningen. Once that
;; has opened, you can use C-c C-r to evaluate the region or C-c C-l to load the
;; whole file.
;;
;; If you don't use Leiningen, you can set inferior-lisp-program to a different REPL command.

(require 'cider)

(eval-after-load 'cider
  '(progn
     (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
     ))


;;; [ ac-nrepl ] --

(require 'ac-nrepl)

(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(eval-after-load 'auto-complete
  '(add-to-list 'ac-modes 'cider-repl-mode))

(add-hook 'clojure-mode-hook
          (lambda ()
            (eval-after-load 'auto-complete
              (add-to-list 'ac-sources 'ac-nrepl))))

(eval-after-load 'cider
  '(define-key cider-mode-map (kbd "M-h") 'ac-nrepl-popup-doc)
  ;; '(define-key my-help-document-prefix-map (kbd "d") 'ac-nrepl-popup-doc)
  )

;;; ac-nrepl
(set-face-attribute 'ac-nrepl-candidate-face nil
                    :foreground "cyan"
                    :bold 'normal)
;; (set-face-attribute 'ac-nrepl-selection-face nil
;;                     )


;;; [ company-cider ]

;; https://github.com/clojure-emacs/company-cider


;;; [ Swank Clojure ] -- Swank Clojure is a server that allows SLIME (the Superior Lisp Interaction Mode for Emacs) to connect to Clojure projects.

;; Deprecated
;; This project is no longer under active development.
;; New users are strongly encouraged to try out nrepl.el instead. If you need an advanced debugger, Ritz might be a better fit.


;;; [ clojure-cheatsheet ]

;; https://github.com/clojure-emacs/clojure-cheatsheet


(provide 'init-my-prog-lang-clojure)

;;; init-my-prog-lang-clojure.el ends here
