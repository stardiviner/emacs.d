;;; init-my-prog-lang-lisp-scheme.el --- init for Scheme
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Scheme Mode ]

(require 'scheme)

;;; [ inferior scheme ]
(setq scheme-program-name "guile")

(with-eval-after-load 'scheme
  (define-key scheme-mode-map (kbd "C-c C-s") 'run-scheme))

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


;;; [ cmuscheme ] -- Scheme process in a buffer. Adapted from tea.el

;; (require 'cmuscheme)


;;; [ geiser ] -- 

(use-package geiser
  :ensure t
  :config
  ;; 'guile, 'racket, 'chicken
  (setq geiser-default-implementation 'guile)

  ;; company-backend
  (add-hook 'scheme-mode-hook
            (lambda ()
              (geiser-mode 1)
              (my-run-geiser-auto)
              (my-company-add-backend-locally 'geiser-company-backend)
              ))

  ;; auto start geiser inferior buffer process `run-geiser'.
  (defun my-run-geiser-auto ()
    (let ((geiser-guile-buffer "* Guile REPL *")
          (geiser-racket-buffer "* Racket REPL *")
          (geiser-chicken-buffer "* Chicken REPL *"))
      (unless (get-buffer geiser-guile-buffer)
        (save-window-excursion
          (run-geiser geiser-default-implementation))
        )
      )
    )

  (with-eval-after-load 'geiser-mode
    (define-key geiser-mode-map (kbd "C-c C-s") 'run-geiser))
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
