;;; init-sly.el --- init for SLY.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ sly ] -- Sylvester the Cat's Common Lisp IDE

(use-package sly
  :ensure t
  :defer t
  :commands (sly sly-mode)
  :custom (;; (sly-auto-start 'always)
           (sly-default-lisp 'sbcl)
           ;; for `ob-lisp'
           (org-babel-lisp-eval-fn #'sly-eval))
  :init
  (add-to-list 'display-buffer-alist '("^\\*sly-mrepl.*\\*" . (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist '("^\\*sly-connections\\*" . (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist '("^\\*sly-threads.*\\*" . (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist '("^\\*sly-description\\*" . (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist '("^\\*sly-apropos.*\\*" . (display-buffer-below-selected)))
  :hook ((sly-mrepl-mode . paredit-mode)
         (sly-mrepl-mode . hl-sexp-mode)
         (sly-mrepl-mode . rainbow-delimiters-mode))
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

  ;; `sly-simple-completions', `sly-flex-completions'
  ;; (setq sly-complete-symbol-function #'sly-simple-completions)
  
  (define-key sly-prefix-map (kbd "M-h") 'sly-documentation-lookup)
  (define-key sly-mrepl-mode-map (kbd "C-c C-k") 'sly-mrepl-clear-recent-output)

  ;; Common Lisp HyperSpec (CLHS)
  ;; (offline archive)
  (setq common-lisp-hyperspec-root
        (concat "file://" (expand-file-name (concat user-emacs-directory "documentations/HyperSpec/HyperSpec/"))))
  ;; use advice to set Emacs default browser to EWW locally on SLY commands.
  (defun hyperspec-lookup--eww-browser (orig-func &rest args)
    (let ((browse-url-browser-function 'eww-browse-url))
      (apply orig-func args)))
  (advice-add 'hyperspec-lookup :around #'hyperspec-lookup--eww-browser))

;;; [ sly-repl-ansi-color ] -- Add ANSI colors support to the sly mrepl.

(use-package sly-repl-ansi-color
  :ensure t
  :defer t
  :after sly
  :config
  (add-to-list 'sly-contribs 'sly-repl-ansi-color 'append)
  (sly-setup sly-contribs))

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

;;; [ sly-asdf ] -- ASDF system support for SLY.

(use-package sly-asdf
  :ensure t
  :defer t
  :after sly
  :config
  (add-to-list 'sly-contribs 'sly-asdf 'append)
  (sly-setup sly-contribs))


(provide 'init-sly)

;;; init-sly.el ends here
