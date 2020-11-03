;;; init-prog-lang-emacs-lisp.el --- init Emacs Lisp for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(put 'emacs-lisp-mode 'derived-mode-parent 'prog-mode)

(add-hook 'emacs-lisp-mode-hook #'(lambda () (add-to-list 'electric-pair-pairs '(?\` . ?\'))))

;; Recompile your elc when saving an elisp file.
;; (add-hook 'after-save-hook
;;           #'(lambda ()
;;               (when (file-exists-p (byte-compile-dest-file buffer-file-name))
;;                 (emacs-lisp-byte-compile)))
;;           'append 'local)

;;; [ sly-el-indent ] -- Use `sly-cl-indent' to indent Emacs Lisp.

;; (use-package sly-el-indent
;;   :quelpa (sly-el-indent :fetcher github :repo "cireu/sly-el-indent")
;;   :commands (sly-el-indent-setup)
;;   :init (add-hook 'emacs-lisp-hook #'sly-el-indent-setup))

;;; [ ob-emacs-lisp ]

(use-package ob-emacs-lisp
  :defer t
  :commands (org-babel-execute:emacs-lisp)
  :config
  (add-to-list 'org-babel-load-languages '(emacs-lisp . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("emacs-lisp" . "el"))

  ;; (add-to-list 'org-babel-default-header-args:emacs-lisp
  ;;              '(:lexical . "yes"))
  (add-to-list 'org-babel-default-header-args:emacs-lisp
               '(:noweb . "yes")))

;;; [ IELM (ELISP interactive) ] -- an REPL for emacs. (Read-Eval-Print-Loop)

(use-package ielm
  :ensure t
  :defer t
  :commands (ielm)
  :custom ((ielm-dynamic-return t)
           (ielm-dynamic-multiline-inputs t))
  :init (add-to-list 'display-buffer-alist '("^\\*ielm\\*" . (display-buffer-below-selected)))
  :hook ((ielm-mode . paredit-mode)
         (ielm-mode . hl-sexp-mode))
  :config
  (defun my/ielm-company-setup ()
    (my-company-add-backend-locally 'company-elisp))
  (add-hook 'ielm-mode-hook #'my/ielm-company-setup))

;;; [ eros ] -- Evaluation Result OverlayS for Emacs Lisp.

(use-package eros
  :ensure t
  :defer t
  :init (eros-mode 1))

;;; [ macrostep ] -- interactive macro-expander for Emacs.

(use-package macrostep
  :ensure t
  :defer t
  :commands (macrostep-expand)
  :init (setq macrostep-expand-in-separate-buffer nil
              macrostep-expand-compiler-macros t))

;;; [ elmacro ] -- display keyboard macros or latest interactive commands as emacs lisp.

;; (use-package elmacro
;;   :ensure t
;;   :defer t
;;   :init (elmacro-mode 1))

;;; [ ERT ] -- Emacs Lisp Regression Testing.

;; (require 'ert)
;; (require 'ert-x)

;;; [ xtest ] -- Simple Testing with Emacs & ERT

;;; [ faceup ] -- Regression test system for font-lock

;;; [ test-simple ] -- Simple Unit Test Framework for Emacs Lisp

;;; [ buttercup ] -- Behavior-Driven Emacs Lisp Testing

;;; [ dash.el ] -- A modern list library for Emacs.

(use-package dash
  :ensure t
  :defer t
  ;; syntax highlighting of dash functions.
  :init (eval-after-load 'dash '(dash-enable-font-lock)))

;;; [ elisp-depmap ] -- A library to generate a dependency map for elisp projects.

(use-package elisp-depmap
  :ensure t
  :commands (elisp-depmap-graphviz-digraph elisp-depmap-graphviz elisp-depmap-makesummarytable))


(provide 'init-prog-lang-emacs-lisp)

;;; init-prog-lang-emacs-lisp.el ends here
