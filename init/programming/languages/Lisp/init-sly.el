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
                  common-lisp-lisp-mode-hook
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
              (unless (sly-connected-p)
                (save-excursion (sly)))))

  ;; enable `sly-mode' in Lisps
  ;;
  ;; (dolist (hook '(sly-mode-hook
  ;;                 sly-mrepl-mode-hook
  ;;                 lisp-mode-hook
  ;;                 lisp-interaction-mode-hook
  ;;                 common-lisp-lisp-mode-hook
  ;;                 ))
  ;;   (add-hook hook 'sly-mode))
  )


;;; [ company-sly ] -- Company-mode completion backend for SLY.

(use-package company-sly
  :ensure t
  :config
  (add-hook 'sly-mode-hook 'sly-company-mode)

  (dolist (hook '(sly-mode-hook
                  sly-mrepl-mode-hook
                  lisp-mode-hook
                  lisp-interaction-mode-hook
                  common-lisp-lisp-mode-hook
                  ))
    (add-hook hook
              (lambda ()
                (my-company-add-backends-to-mode '(sly-company))
                )))
  )


;;; [ sly-macrostep ] -- Expand CL macros inside source files

(use-package sly-macrostep
  :ensure t
  )


(provide 'init-sly)

;;; init-sly.el ends here
