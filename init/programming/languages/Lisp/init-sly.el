;;; init-sly.el --- init for SLY.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ sly ] -- Sylvester the Cat's Common Lisp IDE

(use-package sly
  :ensure t
  :defer t
  :commands (sly)
  :init
  (setq sly-default-lisp 'sbcl)

  ;; enable `sly-mode' in Lisps
  (dolist (hook '(lisp-mode-hook
                  lisp-interaction-mode-hook
                  sly-mrepl-mode-hook
                  ))
    (add-hook hook 'sly-mode))
  :config
  ;; load SLY contribs
  ;; (setq sly-contribs `(sly-fancy
  ;;                      sly-retro
  ;;                      sly-scratch
  ;;                      sly-mrepl
  ;;                      sly-autodoc
  ;;                      ))
  (sly-setup sly-contribs)

  (add-hook 'sly-mrepl-mode-hook #'my-lisp-repl-common-settings)

  (dolist (hook '(sly-mode-hook
                  sly-mrepl-mode-hook
                  lisp-mode-hook
                  lisp-interaction-mode-hook
                  ))
    (add-hook hook
              (lambda ()
                (local-set-key (kbd "C-h d d") 'sly-documentation-lookup)
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

  ;; setup SLY REPL buffer
  (add-hook 'sly-mrepl-mode-hook
            (lambda ()
              ;; (paredit-mode 1)
              (smartparens-strict-mode 1)
              (eldoc-mode 1)))

  ;; [ sly-company ] -- Company-mode completion backend for SLY.
  (use-package sly-company
    :ensure t
    :defer t
    :init
    (dolist (hook '(sly-mode-hook
                    sly-mrepl-mode-hook
                    ))
      (add-hook hook
                (lambda ()
                  (sly-company-mode 1)
                  (my-company-add-backend-locally 'sly-company)
                  )))
    )

  ;; [ ob-lisp ]
  (setq org-babel-lisp-eval-fn #'sly-eval)
  )

;;; [ sly-repl-ansi-color ]

;; (use-package sly-repl-ansi-color
;;   :ensure t
;;   :defer t
;;   :init
;;   (add-to-list 'sly-contribs 'sly-repl-ansi-color)
;;   )


;;; [ sly-macrostep ] -- Expand CL macros inside source files

;;; - [C-c M-e] in `sly-editing-mode'.

(use-package sly-macrostep
  :ensure t
  :defer t)


;;; [ sly-named-readtables ] -- Support named readtables in Common Lisp files.

;; (use-package sly-named-readtables
;;   :ensure t
;;   )


;;; [ sly-quicklisp ] -- Quicklisp support for SLY.

;; (use-package sly-quicklisp
;;   :ensure t
;;   )


(provide 'init-sly)

;;; init-sly.el ends here
