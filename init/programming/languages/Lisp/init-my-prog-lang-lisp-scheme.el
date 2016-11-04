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

(add-hook 'scheme-mode-hook #'my-lisp-common-settings)

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

;; (add-hook 'scheme-mode-hook 'run-scheme-auto-create)


;;; [ cmuscheme ] -- Scheme process in a buffer. Adapted from tea.el

;; (use-package cmuscheme
;;   :ensure t)


;;; [ geiser ] -- Scheme completion.

(use-package geiser
  :ensure t
  :init
  ;; 'guile, 'racket, 'chicken
  (setq geiser-default-implementation 'guile)
  
  ;; company-backend
  (add-hook 'scheme-mode-hook
            (lambda ()
              (geiser-mode 1)
              ;; (my-run-geiser-auto)
              (my-company-add-backend-locally 'geiser-company-backend)
              ))

  :config
  ;; auto start geiser inferior buffer process `run-geiser'.
  (defun my-run-geiser-auto ()
    (interactive)
    (let ((geiser-guile-buffer "* Guile REPL *")
          (geiser-racket-buffer "* Racket REPL *")
          (geiser-chicken-buffer "* Chicken REPL *"))
      (unless (get-buffer geiser-guile-buffer)
        (save-window-excursion
          (run-geiser geiser-default-implementation)))))

  (define-key geiser-mode-map (kbd "C-c C-s") 'my-run-geiser-auto)
  )


;;; [ quack ] -- enhanced Emacs Support for Editing and Running Scheme Code

;; (use-package quack
;;   :ensure t)



(provide 'init-my-prog-lang-lisp-scheme)

;;; init-my-prog-lang-lisp-scheme.el ends here
