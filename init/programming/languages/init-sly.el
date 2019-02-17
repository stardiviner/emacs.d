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
  :config
  ;; setup the `sly-contribs' before starting SLY via [M-x sly].
  (setq sly-contribs `(sly-fancy sly-scratch sly-mrepl sly-autodoc))
  (sly-setup sly-contribs)
  ;; (sly-enable-contrib)

  (mapc
   (lambda (hook) (add-hook hook #'sly-mode))
   '(lisp-mode-hook lisp-interaction-mode-hook sly-mrepl-mode-hook))
  
  (defun my-sly-setup ()
    ;; Autodoc (like `eldoc')
    (sly-autodoc-mode 1)
    (local-set-key (kbd "C-h d d") 'sly-documentation-lookup))
  (mapc
   (lambda (hook) (add-hook hook #'my-sly-setup))
   '(sly-mode-hook sly-mrepl-mode-hook lisp-mode-hook lisp-interaction-mode-hook))

  (add-hook 'sly-mrepl-mode-hook #'my-lisp-repl-common-settings)

  ;; `sly-simple-completions', `sly-flex-completions'
  ;; (setq sly-complete-symbol-function #'sly-simple-completions)
  
  (define-key sly-prefix-map (kbd "M-h") 'sly-documentation-lookup)
  (define-key sly-mrepl-mode-map (kbd "C-c C-k") 'sly-mrepl-clear-recent-output)

  ;; Connecting to SLY automatically
  (setq sly-auto-start 'always)

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

  ;; [ ob-lisp ]
  (require 'ob-lisp)
  (setq org-babel-lisp-eval-fn #'sly-eval))

;;; [ sly-repl-ansi-color ]

(use-package sly-repl-ansi-color
  :ensure t
  :defer t
  :after sly
  :init (add-to-list 'sly-contribs 'sly-repl-ansi-color))

;;; [ sly-macrostep ] -- Expand CL macros inside source files

;;; - [C-c M-e] in `sly-editing-mode'.

(use-package sly-macrostep
  :ensure t
  :defer t
  :after sly)

;;; [ sly-named-readtables ] -- Support named readtables in Common Lisp files.

;; (use-package sly-named-readtables
;;   :ensure t
;;   :defer t
;;   :after sly)


;;; [ sly-quicklisp ] -- Quicklisp support for SLY with command `sly-quickload' / [C-c C-d C-q].

(use-package sly-quicklisp
  :ensure t
  :defer t
  :after sly
  :commands (sly-quickload))


(provide 'init-sly)

;;; init-sly.el ends here
