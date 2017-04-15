;;; init-my-prog-lang-python.el --- init Python Programming Language Environment
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ python-mode ] --- Python's flying circus support for Emacs

(use-package python-mode
  :ensure t
  :defer t
  :config
  (setq python-indent-offset 4
        python-indent 4
        python-indent-guess-indent-offset t
        python-skeleton-autoinsert nil ; auto interactive insert skeleton
        )
  )


;;; [ Inferior Python ]

(require 'python)

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


;;; [ elpy ] -- Emacs Python Development Environment.

(use-package elpy
  :ensure t
  :config
  (setq elpy-rpc-backend "jedi"
        elpy-modules '(elpy-module-sane-defaults
                       ;; elpy-module-company
                       elpy-module-eldoc
                       elpy-module-flymake
                       ;; elpy-module-highlight-indentation
                       elpy-module-pyvenv
                       elpy-module-yasnippet)
        elpy-company-post-completion-function 'elpy-company-post-complete-parens
        )

  (defun my-elpy-settings ()
    (interactive)
    ;; don't use `elpy-company-backend', `capf' works correctly.
    (my-company-add-backend-locally 'elpy-company-backend)
    
    (define-key python-mode-map (kbd "C-h d d") 'elpy-doc)
    (define-key python-mode-map (kbd "M-,") 'pop-tag-mark)
    (define-key python-mode-map (kbd "C-c C-s") 'run-python)
    (define-key elpy-mode-map (kbd "C-c C-s") 'run-python)
    )
  
  (add-hook 'elpy-mode-hook #'my-elpy-settings)

  (add-hook 'python-mode-hook #'elpy-mode)
  )


;;; [ pyenv-mode ] -- Python virtual environment interface

;; (use-package pyenv-mode
;;   :ensure t
;;   :defer t
;;   :init
;;   ;; this pyven-mode is global. [C-c C-u] [C-c C-s]
;;   ;; (add-hook 'python-mode-hook 'pyenv-mode)
;;
;;   :config
;;
;;   ;; projectile integration
;;   (defun projectile-pyenv-mode-set ()
;;     "Set pyenv version matching project name.
;;   Version must be already installed."
;;     (pyenv-mode-set (projectile-project-name)))
;;  
;;   (add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)
;;   )


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


;;; [ virtualenv ]


;;; [ virtualenvwrapper ]


;;; [ conda ] -- work with your conda environments.

;; (use-package conda
;;   :ensure t
;;   :defer t
;;   :config
;;   ;; (setq conda-anaconda-home (concat (getenv "HOME") "/.anaconda3"))
;;   ;; (conda-env-initialize-interactive-shells)
;;   (conda-env-initialize-eshell)
;;   ;; (conda-env-autoactivate-mode t) ; NOTE: this is annoying.
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

(use-package importmagic
  :ensure t
  :init
  (add-hook 'python-mode-hook 'importmagic-mode))

;;; [ Emacs IPython Notebook (EIN) ] -- IPython notebook client in Emacs

(use-package ein
  :ensure t
  :defer t
  :config
  (setq ein:use-auto-complete t
        ;; ein:use-auto-complete-superpack nil
        ein:use-smartrep nil
        ein:load-dev nil
        )
  )

;;; [ ob-ipython ]

(use-package ob-ipython
  :ensure t
  :config
  ;; (setq ob-ipython-command "ipython") ; "jupyter"
  
  ;; open ipython block block with `python-mode'
  ;; (add-to-list 'org-src-lang-modes '("ipython" . python))
  ;; use IJulia backend for IPython notebook
  (add-to-list 'org-src-lang-modes '("ipython" . julia))

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
