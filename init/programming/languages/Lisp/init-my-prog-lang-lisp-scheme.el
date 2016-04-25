;;; init-my-prog-lang-lisp-scheme.el --- init for Scheme
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Scheme Mode ]


;;; [ Inferior Scheme ]

;;; Usage:
;; (run-scheme)

(setq scheme-program-name "guile")


;; auto run `run-scheme' for scheme buffer.
(defun run-scheme-auto-create ()
  "Auto run `run-scheme' when not running."
  (unless (and scheme-buffer
               (get-buffer scheme-buffer)
               (comint-check-proc scheme-buffer))
    (save-window-excursion
      (run-scheme scheme-program-name))
    ;; (switch-to-buffer scheme-buffer)
    ))

(add-hook 'scheme-mode-hook 'run-scheme-auto-create)


;;; [ geiser ] -- 

(use-package geiser
  :ensure t
  :config
  ;; 'guile, 'racket, 'chicken
  (setq geiser-default-implementation 'guile)

  ;; company-backend
  (add-hook 'scheme-mode-hook
            (lambda ()
              (my-company-add-backend-locally 'geiser-company-backend)
              ))
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
