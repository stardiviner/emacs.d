;;; init-my-prog-lang-lisp-scheme.el --- init for Scheme
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Scheme Mode ]


;;; [ Inferior Scheme ]

;;; Usage:
;; (run-scheme)

;; (setq scheme-program-name "scheme")


;;; [ geiser ] -- 

(use-package geiser
  ;; :ensure t
  ;; :config
  ;; (run-geiser)
  )


;;; [ ac-geiser ]

;; (require 'ac-geiser)
;;
;; (dolist (hook '(lisp-mode-hook
;;                 lisp-interaction-mode-hook
;;                 scheme-mode-hook
;;                 ))
;;   (add-hook hook (lambda ()
;;                    (ac-geiser-setup)
;;                    (add-to-list 'ac-sources 'ac-source-geiser))))


;;; [ Quack ] -- enhanced Emacs Support for Editing and Running Scheme Code

;;; http://www.neilvandyke.org/quack/


;;; [ Guile Mode ]

;; (run-guile)


;;; [ GDS ] -- 



(provide 'init-my-prog-lang-lisp-scheme)

;;; init-my-prog-lang-lisp-scheme.el ends here
