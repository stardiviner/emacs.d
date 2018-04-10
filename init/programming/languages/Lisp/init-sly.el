;;; init-sly.el --- init for SLY.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ sly ] -- Sylvester the Cat's Common Lisp IDE

(use-package sly
  :ensure t
  :defer t
  :commands (sly)
  :preface (setq sly-default-lisp 'sbcl)
  :init (add-to-list 'display-buffer-alist
                     '("^\\*sly-mrepl for sbcl\\*"
                       (display-buffer-reuse-window display-buffer-below-selected)))
  :config
  ;; load SLY contribs
  (setq sly-contribs `(sly-fancy sly-scratch sly-mrepl sly-autodoc))
  (sly-setup sly-contribs)

  (mapc
   (lambda (hook) (add-hook hook #'sly-mode))
   '(lisp-mode-hook lisp-interaction-mode-hook sly-mrepl-mode-hook))
  
  (defun my-sly-setup ()
    (local-set-key (kbd "C-h d d") 'sly-documentation-lookup))
  (mapc
   (lambda (hook) (add-hook hook #'my-sly-setup))
   '(sly-mode-hook sly-mrepl-mode-hook lisp-mode-hook lisp-interaction-mode-hook))

  (add-hook 'sly-mrepl-mode-hook #'my-lisp-repl-common-settings)

  (define-key sly-prefix-map (kbd "M-h") 'sly-documentation-lookup)
  (define-key sly-mrepl-mode-map (kbd "C-c C-k") 'sly-mrepl-clear-recent-output)

  ;; auto connect SLY.
  (setq sly-auto-start t)

  (add-to-list 'display-buffer-alist
               '("^\\*sly-mrepl.*\\*" (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist
               '("^\\*sly-connections\\*" (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist
               '("^\\*sly-threads.*\\*" (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist
               '("^\\*sly-description\\*" (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist
               '("^\\*sly-apropos.*\\*" (display-buffer-below-selected)))

  ;; [ sly-company ] -- Company-mode completion backend for SLY.
  (use-package sly-company
    :ensure t
    :config
    (defun my-sly-company-setup ()
      (sly-company-mode 1)
      (my-company-add-backend-locally 'sly-company))
    (mapc
     (lambda (hook) (add-hook hook #'my-sly-company-setup))
     '(sly-mode-hook sly-mrepl-mode-hook))
    )

  ;; [ ob-lisp ]
  (require 'ob-lisp)
  (setq org-babel-lisp-eval-fn #'sly-eval)
  )

;;; [ sly-repl-ansi-color ]

(use-package sly-repl-ansi-color
  :ensure t
  :after sly
  :init
  (add-to-list 'sly-contribs 'sly-repl-ansi-color)
  )


;;; [ sly-macrostep ] -- Expand CL macros inside source files

;;; - [C-c M-e] in `sly-editing-mode'.

(use-package sly-macrostep
  :ensure t)


;;; [ sly-named-readtables ] -- Support named readtables in Common Lisp files.

;; (use-package sly-named-readtables
;;   :ensure t
;;   )


;;; [ sly-quicklisp ] -- Quicklisp support for SLY with command `sly-quickload' / [C-c C-d C-q].

(use-package sly-quicklisp
  :ensure t)


(provide 'init-sly)

;;; init-sly.el ends here
