;;; init-my-prog-lang-python.el --- init Python Programming Language Environment
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ python-mode ] --- Python's flying circus support for Emacs

(use-package python-mode
  :ensure t
  :config
  (setq python-indent-offset 4
        python-indent 4
        python-indent-guess-indent-offset t
        python-skeleton-autoinsert nil ; auto interactive insert skeleton
        )
  )


;;; [ Inferior Python ]

(use-package python
  :ensure t
  :bind (:map python-mode-map
              ("C-c C-s" . run-python))
  :config
  (setq python-shell-interpreter "python" ; "ipython"
        python-shell-completion-native-enable nil
        )

  (case python-shell-interpreter
    ("python"
     (setq python-shell-interpreter-args "-i"))
    ("ipython"
     (setq python-shell-interpreter-args "--simple-prompt --pprint"))
    )

  (defun my-inferior-python ()
    "My function to start or switch to inferior-python process buffer `PROCESS-BUFFER-NAME'."
    (interactive)
    (unless (get-buffer-process "*Python*")
      (run-python "python"))
    (switch-to-buffer "*Python*")
    )

  (unless (boundp 'my-prog-inferior-map)
    (define-prefix-command 'my-prog-inferior-map))
  (define-key my-prog-inferior-map (kbd "p") 'my-inferior-python) ; 'run-python
  )

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
    (interactive)
    ;; don't use `elpy-company-backend', `capf' works correctly.
    (my-company-add-backend-locally 'elpy-company-backend)
    )
  
  (add-hook 'elpy-mode-hook #'my-elpy-company-setup)
  )

;;; [ pyvenv ] -- Python virtual environment interface for Emacs.

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-workon "python3")
  (add-hook 'python-mode-hook #'pyvenv-mode)
  )


;;; [ pythonic ]

;; (use-package pythonic
;;   :ensure t
;;   :defer t
;;   :init
;;   (pythonic-activate "~/.virtualenvs/python3")
;;   )


;;; [ pygen ] -- Python code generation in Emacs with Elpy and python-mode.

(use-package pygen
  :ensure t
  :init
  (add-hook 'python-mode-hook 'pygen-mode))


;;; [ cinspect ] -- Use cinspect to look at the CPython source of builtins and other C objects!

;; (use-package cinspect
;;   :ensure t
;;   :defer t
;;   :init
;;   (add-hook 'python-mode-hook 'cinspect)
;;   )

;;; [ importmagic ] -- An Emacs package that resolves unimported Python symbols.

;; (use-package importmagic
;;   :ensure t
;;   :init
;;   (add-hook 'python-mode-hook 'importmagic-mode))

;;; [ Emacs IPython Notebook (EIN) ] -- IPython notebook client in Emacs

(use-package ein
  :ensure t)

;;; [ ob-ipython ]

(use-package ob-ipython
  :ensure t
  :config
  ;; open ipython block block with `python-mode'
  ;; (add-to-list 'org-src-lang-modes '("ipython" . python))
  ;; use IJulia backend for IPython notebook
  ;; (add-to-list 'org-src-lang-modes '("ipython" . python))

  (setq org-babel-default-header-args:ipython
        '((:session . nil)
          ;; (:dir . "data/images")
          (:exports . "both")
          ))

  ;; different kernels support
  (defun ob-ipython-kernel-get-kernels ()
    "Get available Jupyter kernels.
This can be useful for snippets to select kernel interactively."
    (let ((kernels (split-string
                    (shell-command-to-string
                     "jupyter-kernelspec list | sed '1d' | awk -F ' ' '{print $1}'"))))
      ;; (completing-read "Jupyter kernels: "
      ;;                  kernels)
      kernels
      )
    )
  )


(provide 'init-my-prog-lang-python)

;;; init-my-prog-lang-python.el ends here
