;;; init-prog-lang-python.el --- init Python Programming Language Environment
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ python ] -- (built-in) Python's flying circus support for Emacs.

(use-package python
  :ensure t
  :defer t
  :commands (run-python python-mode)
  :bind (:map python-mode-map ("C-c C-s" . run-python))
  :custom ((python-shell-interpreter "python")
           (python-shell-completion-native-enable nil))
  :init
  (cl-case python-shell-interpreter
    ("python"
     (setq python-shell-interpreter-args "-i"))
    ("ipython"
     (setq python-shell-interpreter-args "--simple-prompt --pprint")
     (setenv "IPY_TEST_SIMPLE_PROMPT" "1")))
  (add-to-list 'display-buffer-alist '("^\\*Python\\*" . (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist '("^\\*Python Doc\\*" . (display-buffer-below-selected)))
  :config
  (add-hook 'python-mode-hook #'electric-pair-local-mode)
  (add-hook 'python-mode-hook #'flymake-mode-off 'append))

;;; [ ob-python ]

(use-package ob-python
  :defer t
  :commands (org-babel-execute:python)
  :config
  (add-to-list 'org-babel-load-languages '(python . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("python" . "py"))

  (add-to-list 'org-babel-default-header-args:python
               '(:eval . "yes"))
  (add-to-list 'org-babel-default-header-args:python
               '(:noweb . "yes"))
  (add-to-list 'org-babel-default-header-args:python
               '(:results . "output")))

;;; [ lsp-python ] -- Python support for lsp-mode with pyls.

(use-package lsp-mode
  :ensure t
  :defer t
  :hook ((python-mode . lsp)
         (python-mode . dap-mode)
         (python-mode . dap-ui-mode))
  :config (require 'dap-python))

;;; [ lsp-python-ms ] -- Emacs lsp-mode client for Microsoft's python language server.

;; (use-package lsp-python-ms
;;   :ensure t
;;   :demand t
;;   :preface
;;   (require 'lsp-python-ms)
;;   (unless (member 'pyls lsp-disabled-clients)
;;     (push 'pyls lsp-disabled-clients))
;;   :hook (python-mode . lsp-deferred))

;;; [ pyvenv ] -- Python virtual environment interface for Emacs.

;; (use-package pyvenv
;;   :ensure t
;;   :init (pyvenv-workon (if-let (virtual_env (getenv "VIRTUAL_ENV"))
;;                            (car (last (split-string virtual_env "/")))
;;                          "3.8"))
;;   :hook (python-mode . pyvenv-mode))

;;; [ poetry ] -- Python dependency management and packaging in Emacs.

;; (use-package poetry
;;   :ensure t
;;   :defer t
;;   :commands (poetry)
;;   :hook (python-mode . poetry-tracking-mode))

;;; [ live-py-mode ] -- Python Live Coding in Emacs.

(use-package live-py-mode
  :ensure t
  :defer t
  :commands (live-py-mode))

;;; [ pygen ] -- Python code generation in Emacs with Elpy and python-mode.

;; (use-package pygen
;;   :ensure t
;;   :init
;;   (add-hook 'python-mode-hook 'pygen-mode))

;;; [ elpygen ] -- Generate a Python function/method using a symbol under point.

;;; [ cinspect ] -- Use cinspect to look at the CPython source of builtins and other C objects!

;; (use-package cinspect
;;   :ensure t
;;   :defer t
;;   :init (add-hook 'python-mode-hook 'cinspect))


(provide 'init-prog-lang-python)

;;; init-prog-lang-python.el ends here
