;;; init-prog-lang-scheme.el --- init for Scheme
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Scheme Mode ]

(use-package scheme
  :ensure t
  :defer t
  :custom (scheme-program-name "guile")
  :commands (run-scheme)
  :init
  (add-to-list 'display-buffer-alist '("\\* Guile REPL \\*" . (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist '("\\* Racket REPL \\*" . (display-buffer-below-selected)))
  :config
  (add-hook 'scheme-mode-hook #'my-lisp-common-settings)
  (define-key scheme-mode-map (kbd "C-c C-s") 'run-scheme)
  
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
  )

;;; [ geiser ] -- Scheme completion.

(use-package geiser
  :ensure t
  :defer t
  :commands (run-geiser run-guile)
  ;; 'guile, 'racket, 'chicken
  :custom ((geiser-default-implementation 'guile))
  :hook (scheme-mode . geiser-mode)
  :config
  ;; company-backend
  (defun my/geiser-company-setup ()
    (my-company-add-backend-locally 'geiser-company-backend))
  (add-hook 'scheme-mode-hook #'my/geiser-company-setup)
  ;; auto start geiser inferior buffer process `run-geiser'.
  (defun my-run-geiser-auto ()
    (interactive)
    (let ((geiser-guile-buffer "* Guile REPL *")
          (geiser-racket-buffer "* Racket REPL *")
          (geiser-chicken-buffer "* Chicken REPL *"))
      (unless (get-buffer geiser-guile-buffer)
        (save-window-excursion
          (run-geiser geiser-default-implementation)))))
  (define-key scheme-mode-map (kbd "C-c C-s") 'my-run-geiser-auto))

;;; [ quack ] -- enhanced Emacs Support for Editing and Running Scheme Code

;; (use-package quack
;;   :ensure t
;;   :defer t)


;;; [ ob-scheme ]

(use-package ob-scheme
  :defer t
  :commands (org-babel-execute:scheme)
  :config
  (add-to-list 'org-babel-load-languages '(scheme . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("scheme" . "scm"))
  ;; FIXME: ob-scheme does not use already opened existing REPL session.
  (pcase scheme-program-name
    ("guile"
     (add-to-list 'org-babel-default-header-args:scheme '(:session . "* Guile REPL *")))
    ("racket"
     (add-to-list 'org-babel-default-header-args:scheme '(:session . "* Racket REPL *")))))


(provide 'init-prog-lang-scheme)

;;; init-prog-lang-scheme.el ends here
