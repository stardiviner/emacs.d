;;; init-my-prog-lang-C-common.el --- C-like programming languages common init.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ CC-mode ]

;; cc-mode provides:
;; - c-mode
;; - c++-mode
;; - java-mode
;; - objc-mode
;; - idl-mode
;; - pike-mode
;; - awk-mode
;; - and some 3rd part modes.

;; (require 'cc-mode)

;; Do not check for old-style (K&R) function declarations; this speeds up
;; indenting a lot.
(setq c-recognize-knr-p nil)

;; Hook called by all CC Mode modes for common initializations.
;; (add-hook 'c-mode-common-hook)

(defvar c-dialects-mode
  '(c-mode
    c++-mode
    objc-mode
    ))


;; [ C-mode ]

;; indent
(setq-default c-syntactic-indentation t
              c-basic-offset 2
              tab-width 2
              indent-tabs-mode nil ; never use tab, always use space only.
              tab-always-indent t ; make tab key always call a indent command.
              )

(setq-default c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (other . "gnu")))


;;; [ Irony-mode ] --- A C/C++ minor mode for Emacs powered by libclang.

(use-package irony
  :ensure t
  :defer t
  :init
  (hook-modes c-dialects-mode
    (when (memq major-mode irony-supported-major-modes)
      (irony-mode 1))

    (c-toggle-auto-hungry-state 1)
    ;; (c-toggle-auto-newline 1)
    ;; (c-toggle-hungry-state 1)
    (electric-indent-mode 1)
    )

  :config
  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  )


;;; [ company-irony-c-headers ]

(use-package company-irony-c-headers
  :ensure t)


;;; [ company-irony ]

(use-package company-irony
  :ensure t
  :defer t
  :init
  (defun company-irony-add ()
    ;; (optional) adds CC special commands to `company-begin-commands'
    ;; in order to trigger completion at interesting places, such as
    ;; after scope operator.
    ;;     std::|
    (company-irony-setup-begin-commands)

    (make-local-variable 'company-backends)
    (add-to-list 'company-backends
                 '(company-irony-c-headers
                   company-irony
                   ;; company-semantic
                   ;; company-gtags
                   ;; company-etags
                   :with
                   company-yasnippet))
    )
  
  (hook-modes c-dialects-mode
    (company-irony-add))
  )


;;; [ irony-eldoc ]

(use-package irony-eldoc
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'irony
    (add-hook 'irony-mode-hook #'irony-eldoc))
  )


;;; [ flycheck-irony ]

(use-package flycheck-irony
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'irony
    (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
  )


;;; [ flycheck-cstyle ] --

(use-package flycheck-cstyle
  :ensure t
  :defer t
  :init
  (flycheck-cstyle-setup)
  (flycheck-add-next-checker 'c/c++-cppcheck '(warning . cstyle))
  ;; (flycheck-add-next-checker 'c/c++-clang '(warning . cstyle))
  )


;;; [ gccsense ]

;; (use-package gccsense
;;   :ensure t)



(provide 'init-my-prog-lang-C-common)

;;; init-my-prog-lang-C-common.el ends here
