;;; init-prog-lang-C-common.el --- C-like programming languages common init.
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

(hook-modes c-dialects-mode
  ;; (c-toggle-auto-hungry-state 1)
  ;; (c-toggle-auto-newline 1)
  ;; (c-toggle-hungry-state 1)
  (electric-indent-mode 1)
  )

(setq-default c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (other . "gnu")))

(defun my/c-mode-common-header-switch ()
  "Open header file at point."
  (local-set-key (kbd "C-c C-o") 'ff-find-other-file))
(add-hook 'c-mode-common-hook #'my/c-mode-common-header-switch)


;;; [ ob-C ]

(require 'ob-C)

(add-to-list 'org-babel-load-languages '(C . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(add-to-list 'org-babel-tangle-lang-exts '("C" . "c"))

(add-to-list 'org-babel-default-header-args:C
             '(:results . "output"))

;; (setq org-babel-C++-compiler "g++") ; "g++", "c++", "g++ -v"
(add-to-list 'org-babel-tangle-lang-exts '("C++" . "cpp"))

;; (add-to-list 'org-babel-default-header-args:C++
;;              '(:results . "output"))


;;; [ Irony-mode ] --- A C/C++ minor mode for Emacs powered by libclang.

(use-package irony
  :ensure t
  :ensure-system-package clang
  :defer t
  :init
  (hook-modes c-dialects-mode
    (when (memq major-mode '(c-mode c++-mode objc-mode))
      (irony-mode 1)))
  :config
  ;; find the compile flag options automatically:
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

  ;; [ company-irony ]
  (use-package company-irony
    :ensure t
    :init
    ;; [ company-irony-c-headers ]
    (use-package company-irony-c-headers
      :ensure t)

    (defun company-irony-add ()
      ;; (optional) adds CC special commands to `company-begin-commands'
      ;; in order to trigger completion at interesting places, such as
      ;; after scope operator.
      ;;     std::|
      (company-irony-setup-begin-commands)

      (make-local-variable 'company-backends)
      (add-to-list 'company-backends
                   '(company-irony
                     :with
                     company-yasnippet))
      (add-to-list 'company-backends 'company-irony-c-headers)
      )

    (hook-modes c-dialects-mode
      (when (memq major-mode '(c-mode c++-mode objc-mode))
        (company-irony-add)))
    )

  ;; [ irony-eldoc ]
  (use-package irony-eldoc
    :ensure t
    :after irony
    :init (add-hook 'irony-mode-hook #'irony-eldoc))

  ;; [ flycheck-irony ]
  (use-package flycheck-irony
    :ensure t
    :after irony
    :init (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
  )

;;; [ cquery ] -- Low-latency language server supporting multi-million line C++ code-bases, powered by libclang.

(use-package cquery
  :ensure t
  :init
  (defun my:cquery-setup ()
    (when
        (or (locate-dominating-file default-directory "compile_commands.json")
            (locate-dominating-file default-directory ".cquery"))
      (lsp-cquery-enable)))
  (add-hook 'c-mode-common-hook #'my:cquery-setup))


;;; [ ccls ] -- C/C++/Objective-C lang server support for lsp-mode using Clang.

(use-package ccls
  :defines projectile-project-root-files-top-down-recurring
  :ensure t
  ;; :ensure-system-package ((ccls . "aurman -S ccls"))
  :after lsp
  :commands lsp-ccls-enable
  :hook ((c-mode c++-mode objc-mode) . lsp)
  :config (with-eval-after-load 'projectile
            (setq projectile-project-root-files-top-down-recurring
                  (append '("compile_commands.json" ".ccls")
                          projectile-project-root-files-top-down-recurring))))

;;; [ c-eldoc ] -- helpful description of the arguments to C functions.

(use-package c-eldoc
  :ensure t
  :defer t)

;;; [ flycheck-cstyle ] --

;; (use-package flycheck-cstyle
;;   :ensure t
;;   :defer t
;;   :after flycheck
;;   :config
;;   (flycheck-cstyle-setup)
;;   (flycheck-add-next-checker 'c/c++-cppcheck '(warning . cstyle))
;;   ;; (flycheck-add-next-checker 'c/c++-clang '(warning . cstyle))
;;   )

;;; [ flycheck-clang-analyzer ] -- Integrate Clang Static Analyzer with flycheck for on-the-fly static analysis in Emacs.

;; (use-package flycheck-clang-analyzer
;;   :ensure t
;;   :defer t
;;   :after flycheck
;;   :config (flycheck-clang-analyzer-setup))



(provide 'init-prog-lang-C-common)

;;; init-prog-lang-C-common.el ends here
