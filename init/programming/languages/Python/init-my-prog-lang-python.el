;;; init-my-prog-lang-python.el --- init Python Programming Language Environment
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ python-mode ] --- Python's flying circus support for Emacs

;; (use-package python-mode
;;   :ensure t
;;   :config
;;   (setq-default python-indent-offset 2
;;                 python-indent-guess-indent-offset t
;;                 ;; "python", "ipython", "bpython"
;;                 ;; python-shell-interpreter "python"
;;                 ;; python-shell-interpreter-args "-i"
;;                 ;; python-shell-interpreter-interactive-arg "-i"
;;                 python-shell-virtualenv-path "~/.virtualenvs/python3/"
;;                 ;; python-ffap-setup-code
;;                 ;; python-ffap-string-code
;;                 python-skeleton-autoinsert t
;;                 )
;;
;;   ;; TODO:
;;   ;; temporary solution to fix python-mode completion suspend for long time.
;;   ;; `completion-at-point-functions': (python-completion-complete-at-point t)
;;   (dolist (hook '(python-mode-hook
;;                   inferior-python-mode-hook
;;                   ))
;;     (add-hook hook '(lambda ()
;;                       (setq-local completion-at-point-functions nil))))
;;   )


;;; [ elpy ] -- Emacs Python Development Environment.

(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (setq elpy-rpc-backend "jedi")

  (setq python-indent 4)
  
  (add-hook 'elpy-mode-hook
            (lambda ()
              (my-company-add-backends-to-mode '(elpy-company-backend))
              (define-key my-prog-help-document-map (kbd "d") 'elpy-doc)
              ))
  )


;;; [ pydoc ]

;; (use-package pydoc
;;   :ensure t
;;   :config
;;   (define-key my-prog-help-document-map (kbd "d") 'pydoc)
;;   )


;;; [ Inferior Python ]

;; - [C-c C-l] :: `python-shell-send-file'
;; - [C-c C-r] :: `python-shell-send-region'
;; - [C-c C-c] :: `python-shell-send-buffer'
;; - [C-M-x]   :: `python-shell-send-defun'
;; - `python-shell-send-string'

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

;; (define-key my-prog-inferior-map (kbd "p") 'run-python)
(define-key my-prog-inferior-map (kbd "p") 'my-inferior-python)

(add-hook 'python-mode-hook
          (lambda ()
            ;; inferior-python
            (define-key python-mode-map (kbd "C-c C-s") 'run-python)
            ))
;; (setq inferior-python-mode-hook '(python-shell-send-setup-code))
;; (setq inferior-python-mode-hook nil)

(add-hook 'inferior-python-mode-hook
          '(lambda ()
             (my-company-add-backends-to-mode '(company-anaconda))))


;;; [ pyenv-mode ] -- Python virtual environment interface

;;; Usage:
;;
;; 1. [M-x pyenv-mode-set] :: specify pyenv python version.
;; 2. [M-x run-python] :: run inferior python.
;; 3. [M-x pyenv-mode-unset] :: unset

;; (use-package pyenv-mode
;;   :ensure t
;;   :config
;;   ;; FIXME: this pyven-mode is global. [C-c C-u] [C-c C-s]
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

;;; This is a simple global minor mode which will replicate the changes done by
;;; virtualenv activation inside Emacs.

;;; Usage:
;;
;; - `pyvenv-activate'
;;
;;   queries the user for a virtual environment directory to activate.
;;
;; - `pyvenv-workon'
;;
;;    queries for a virtual environment in $WORKON_HOME (from
;;    virtualenvwrapper.sh).
;;
;; - `pyvenv-mode-line-indicator'
;;
;;   an indicator for mode-line.

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-workon "python3"))


;;; [ pythonic ]

;; - `pythonic-activate' :: active Python virtualenv.

;; (use-package pythonic
;;   :ensure t
;;   :config
;;   (pythonic-activate "~/.virtualenvs/python3")
;;   )


;;; [ virtualenv ]


;;; [ virtualenvwrapper ]


;;; [ jedi.el ] --- a python auto-completion library.

;; (use-package jedi
;;   :ensure t
;;   :config
;;   (setq jedi:tooltip-method '(pos-tip popup)
;;         jedi:complete-on-dot t
;;         jedi:use-shortcuts t)
;;
;;   (add-hook 'python-mode-hook 'jedi:setup)
;;   ;; use auto-complete and use jedi as ac source.
;;   ;; (add-hook 'python-mode-hook 'jedi:ac-setup)
;;   )


;;; [ company-jedi ]

;; (use-package company-jedi
;;   :ensure t
;;   :config
;;   (add-hook 'python-mode-hook
;;             (lambda ()
;;               (jedi:start-server)
;;
;;               ;; (company-jedi--setup)
;;               (my-company-add-backends-to-mode
;;                '(company-jedi
;;                  ))
;;               ))
;;   )


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
;;             '(lambda ()
;;                (my-company-add-backends-to-mode '(company-anaconda))))
;;   )


;;; [ IPython ]



;;; [ ein ] -- IPython notebook client in Emacs




(provide 'init-my-prog-lang-python)

;;; init-my-prog-lang-python.el ends here
