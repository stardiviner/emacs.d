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
  :config
  (hook-modes c-dialects-mode
    (when (memq major-mode irony-supported-major-modes)
      (irony-mode 1))

    (c-toggle-auto-hungry-state 1)
    ;; (c-toggle-auto-newline 1)
    ;; (c-toggle-hungry-state 1)
    (electric-indent-mode 1)
    )

  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  ;; (add-hook 'irony-mode-hook
  ;;           (lambda ()
  ;;             (define-key irony-mode-map [remap completion-at-point]
  ;;               'irony-completion-at-point-async)
  ;;             (define-key irony-mode-map [remap complete-symbol]
  ;;               'irony-completion-at-point-async)
  ;;             ))
  )


;;; [ company-irony-c-headers ]

(use-package company-irony-c-headers
  :ensure t)


;;; [ company-irony ]

(use-package company-irony
  :ensure t
  :config
  
  (defun company-irony-add ()
    ;; (optional) adds CC special commands to `company-begin-commands'
    ;; in order to trigger completion at interesting places, such as
    ;; after scope operator.
    ;;     std::|
    (company-irony-setup-begin-commands)

    ;; - `company-cmake'
    ;; - `company-clang'
    ;; - `company-semantic'
    ;; - `company-etags'
    ;; - `company-gtags'
    (make-local-variable 'company-backends)
    (add-to-list 'company-backends '(company-irony-c-headers
                                     company-irony
                                     :with
                                     company-yasnippet))
    )

  (add-hook 'c-mode-common-hook 'company-irony-add)
  ;; (hook-modes c-dialects-mode
  ;;   (company-irony-add))
  )


;;; [ irony-eldoc ]

(use-package irony-eldoc
  :ensure t
  :config
  (with-eval-after-load 'irony
    (add-hook 'irony-mode-hook #'irony-eldoc))
  )


;;; [ flycheck-irony ]

(use-package flycheck-irony
  :ensure t
  :config
  (with-eval-after-load 'irony
    (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
  )


;;; [ company-clang ]

;; (use-package company-clang
;;   :ensure t
;;   :config
;;   ;; (setq company-clang-arguments '("-std=c++14")
;;   ;;       company-clang-prefix-guesser 'company-clang-guess-prefix
;;   ;;       )
;;
;;   (setq company-clang-begin-after-member-access t)
;;
;;   (hook-modes c-dialects-mode
;;     (my-company-add-backend-locally 'company-clang))
;;   )


;;; [ company-c-headers ]

;; (use-package company-c-headers
;;   :ensure t
;;   :config
;;   (hook-modes c-dialects-mode
;;     (my-company-add-backend-locally 'company-c-headers))
;;   )


;;; [ gccsense ]

;; (unless (package-installed-p 'gccsense)
;;   (package-install 'gccsense))
;; (require 'gccsense)



;;; [ function-args ] -- showing an inline arguments hint for the C/C++ function at point.

;; (use-package function-args
;;   :ensure t
;;   :config
;;
;;   (autoload 'turn-on-function-args-mode "function-args" nil t)
;;
;;   (add-hook 'c-mode-hook 'turn-on-function-args-mode)
;;   (add-hook 'c++-mode-hook 'turn-on-function-args-mode)
;;
;;
;;   ;; Put c++-mode as default for *.h files (improves parsing):
;;   ;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;;   ;;
;;   ;; Enable case-insensitive searching:
;;   ;; (set-default 'semantic-case-fold t)
;;   ;;
;;   ;; If your includes aren't located in default dirs e.g. /usr/include/ etc, then
;;   ;; you have to do something like this:
;;   ;; (semantic-add-system-include "~/Software/deal.II/include/" 'c++-mode)
;;   ;; (semantic-add-system-include "/usr/local/boost_1_54_0/" 'c++-mode)
;;   ;;
;;   ;; You can add this to improve the parse of macro-heavy code:
;;   ;; (require 'semantic/bovine/c)
;;   ;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file
;;   ;;              "/usr/lib/gcc/x86_64-linux-gnu/4.8/include/stddef.h")
;;   )


(provide 'init-my-prog-lang-C-common)

;;; init-my-prog-lang-C-common.el ends here
