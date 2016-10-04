;;; init-my-prog-lang-python.el --- init Python Programming Language Environment
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ python-mode ] --- Python's flying circus support for Emacs

(use-package python-mode
  :ensure t
  :config
  (setq-default python-indent-offset 2
                python-indent 2
                python-indent-guess-indent-offset t
                ;; "python", "ipython", "bpython"
                ;; python-shell-interpreter "python"
                ;; python-shell-interpreter-args "-i"
                ;; python-shell-interpreter-interactive-arg "-i"
                ;; python-shell-virtualenv-path "~/.virtualenvs/python3/"
                ;; python-ffap-setup-code
                ;; python-ffap-string-code
                python-skeleton-autoinsert t
                )
  )


;;; [ Inferior Python ]

(defun my-inferior-python ()
  "My function to start or switch to inferior-python process buffer `PROCESS-BUFFER-NAME'."
  (interactive)
  (if (get-buffer-process "*Python*")
      ;; the inferior Python process exist
      (switch-to-buffer "*Python*")
    ;; create a new inferior Python process
    (run-python "python")
    ;; kill old process
    ;; (kill-process (get-buffer-process (or process-buffer-name "*Python*"))
    )
  )

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
  )


;;; [ pyenv-mode ] -- Python virtual environment interface

;; (use-package pyenv-mode
;;   :ensure t
;;   :config
;;   ;; this pyven-mode is global. [C-c C-u] [C-c C-s]
;;   ;; (add-hook 'python-mode-hook 'pyenv-mode)
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
  (pyvenv-workon "python3"))


;;; [ pythonic ]

;; (use-package pythonic
;;   :ensure t
;;   :config
;;   (pythonic-activate "~/.virtualenvs/python3")
;;   )


;;; [ virtualenv ]


;;; [ virtualenvwrapper ]


;;; [ anaconda-mode ]

;; (use-package anaconda-mode
;;   :ensure t
;;   :config
;;   ;; enable anaconda-mode in python-mode.
;;   (add-hook 'python-mode-hook 'anaconda-mode)
;;   (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
;;   )


;;; [ company-anaconda ]

;; (use-package company-anaconda
;;   :ensure t
;;   :config
;;   (add-hook 'python-mode-hook
;;             (lambda ()
;;               (my-company-add-backend-locally 'company-anaconda)
;;               ))
;;   )


;;; [ conda ] -- work with your conda environments.

;; (use-package conda
;;   :ensure t
;;   :config
;;   ;; (setq conda-anaconda-home (concat (getenv "HOME") "/.anaconda3"))
;;   ;; (conda-env-initialize-interactive-shells)
;;   (conda-env-initialize-eshell)
;;   ;; (conda-env-autoactivate-mode t) ; NOTE: this is annoying.
;;   )


;;; [ IPython ]



;;; [ Emacs IPython Notebook (EIN) ] -- IPython notebook client in Emacs

(use-package ein
  :ensure t
  :config
  (setq ein:use-auto-complete t
        ;; ein:use-auto-complete-superpack nil
        ein:use-smartrep nil
        ein:load-dev nil
        )
  )



(provide 'init-my-prog-lang-python)

;;; init-my-prog-lang-python.el ends here
