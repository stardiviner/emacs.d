;;; init-sly.el --- init for SLY.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ sly ] -- Sylvester the Cat's Common Lisp IDE

(use-package sly
  :ensure t
  :init
  ;; (require 'sly-autoloads)
  :commands (sly)
  :config
  (setq sly-lisp-implementations
        '((cmucl ("cmucl" "-quiet"))
          ;; (cmucl ("/opt/cmucl/bin/lisp" "-quiet") :init sly-init-command)
          (acl (\"acl7\") :coding-system emacs-mule)
          (sbcl ("/usr/bin/sbcl") :coding-system utf-8-unix)))

  (setq sly-default-lisp 'sbcl)

  (setq sly-contribs '(sly-fancy sly-retro
                                 sly-scratch
                                 sly-mrepl
                                 sly-autodoc))

  (dolist (hook '(sly-mode-hook
                  sly-mrepl-mode-hook
                  lisp-mode-hook
                  lisp-interaction-mode-hook
                  ))
    (add-hook hook
              (lambda ()
                (define-key my-prog-help-document-map (kbd "d") 'sly-documentation-lookup)
                )))
  
  (eval-after-load 'sly
    `(define-key sly-prefix-map (kbd "M-h") 'sly-documentation-lookup))

  (eval-after-load 'sly-mrepl
    `(define-key sly-mrepl-mode-map (kbd "C-c C-k")
       'sly-mrepl-clear-recent-output))

  ;; notify user after SLY connected.
  (add-hook 'sly-connected-hook
            (lambda ()
              (notifications-notify :title "SLY subprocess"
                                    :body "SLY connected")))

  ;; auto connect SLY.
  (add-hook 'sly-mode-hook
            (lambda ()
              (if (equal major-mode 'lisp-mode)
                  (unless (sly-connected-p)
                    (save-excursion (sly))))))

  ;; enable `sly-mode' in Lisps
  (dolist (hook '(lisp-mode-hook
                  lisp-interaction-mode-hook
                  sly-mrepl-mode-hook
                  ))
    (add-hook hook 'sly-mode))

  ;; setup SLY REPL buffer
  (add-hook 'sly-mrepl-mode-hook
            '(lambda ()
               (paredit-mode 1)
               (eldoc-mode 1)))
  )


;;; [ company-sly ] -- Company-mode completion backend for SLY.

(use-package company-sly
  :ensure t
  :config
  ;; (add-hook 'sly-mode-hook 'sly-company-mode)
  (dolist (hook '(sly-mode-hook
                  sly-mrepl-mode-hook
                  ))
    (add-hook hook
              '(lambda ()
                 (my-company-add-backends-to-mode '(sly-company)))))
  )


;;; [ sly-repl-ansi-color ]

(use-package sly-repl-ansi-color
  :ensure t
  :config
  (add-to-list 'sly-contribs 'sly-repl-ansi-color)
  )


;;; [ sly-macrostep ] -- Expand CL macros inside source files

(use-package sly-macrostep
  :ensure t
  )


;;; [ sly- ] -- Support named readtables in Common Lisp files.

(use-package sly-named-readtables
  :ensure t
  )


;;; [ ob-sly ] -- org-babel functions for common lisp evaluation with SLY.

(use-package ob-sly
  :ensure t)


(provide 'init-sly)

;;; init-sly.el ends here
