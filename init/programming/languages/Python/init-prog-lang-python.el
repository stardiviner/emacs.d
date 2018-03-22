;;; init-prog-lang-python.el --- init Python Programming Language Environment
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ python-mode ] --- Python's flying circus support for Emacs

(use-package python-mode
  :ensure t
  :ensure-system-package python
  :preface
  (setq python-indent-offset 4
        python-indent 4
        python-indent-guess-indent-offset t)
  :config
  (setq python-skeleton-autoinsert nil ; auto interactive insert skeleton
        )
  )


;;; [ Inferior Python ]

(use-package python
  :ensure t
  :defer t
  :commands (run-python)
  :bind (:map python-mode-map ("C-c C-s" . run-python))
  :init
  (setq python-shell-interpreter "python")
  (case python-shell-interpreter
    ("python"
     (setq python-shell-interpreter-args "-i"))
    ("ipython"
     (setq python-shell-interpreter-args "--simple-prompt --pprint"))
    )
  :config
  (setq python-shell-completion-native-enable nil)

  (add-to-list 'display-buffer-alist
               '("^\\*Python\\*" (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist
               '("^\\*Python Doc\\*" (display-buffer-below-selected)))
  )


;;; [ ob-python ]

(require 'ob-python)

(add-to-list 'org-babel-load-languages '(python . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(add-to-list 'org-babel-tangle-lang-exts '("python" . "py"))

(add-to-list 'org-babel-default-header-args:python
             '(:eval . "yes"))
(add-to-list 'org-babel-default-header-args:python
             '(:noweb . "yes"))
(add-to-list 'org-babel-default-header-args:python
             '(:results . "output"))
;; (add-to-list 'org-babel-default-header-args:python
;;              '(:session . "*Python*"))


;;; [ elpy ] -- Emacs Python Development Environment.

(use-package elpy
  :ensure t
  :init
  (add-hook 'python-mode-hook #'elpy-mode)
  :bind (:map python-mode-map
              ("C-h d d" . elpy-doc)
              ("M-," . pop-tag-mark))
  :config
  (setq elpy-rpc-backend "jedi"
        elpy-modules '(elpy-module-sane-defaults
                       ;; elpy-module-company
                       elpy-module-eldoc
                       ;; elpy-module-flymake
                       ;; elpy-module-highlight-indentation
                       elpy-module-pyvenv
                       elpy-module-yasnippet
                       ;; elpy-module-django
                       )
        elpy-company-post-completion-function 'elpy-company-post-complete-parens
        )

  (defun my-elpy-company-setup ()
    ;; don't use `elpy-company-backend', `company-capf' works correctly.
    (my-company-add-backend-locally 'elpy-company-backend))
  (add-hook 'elpy-mode-hook #'my-elpy-company-setup)
  )

;;; [ pyvenv ] -- Python virtual environment interface for Emacs.

(use-package pyvenv
  :ensure t
  :defer t
  :init (add-hook 'python-mode-hook #'pyvenv-mode)
  :config (pyvenv-workon "python3.6"))

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
;;   :init
;;   (add-hook 'python-mode-hook 'cinspect)
;;   )


(provide 'init-prog-lang-python)

;;; init-prog-lang-python.el ends here
