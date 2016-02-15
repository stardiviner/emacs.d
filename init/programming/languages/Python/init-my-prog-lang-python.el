;;; init-my-prog-lang-python.el --- init Python Programming Language Environment
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ python-mode ] --- Python's flying circus support for Emacs

(use-package python-mode
  :ensure t
  :config

  (setq-default python-indent-offset 2
                python-indent-guess-indent-offset t
                ;; "python", "ipython", "bpython"
                ;; python-shell-interpreter "python"
                ;; python-shell-interpreter-args "-i"
                ;; python-shell-interpreter-interactive-arg "-i"
                python-shell-virtualenv-path "~/.virtualenvs/python3/"
                ;; python-ffap-setup-code
                ;; python-ffap-string-code
                python-skeleton-autoinsert t
                )

  ;; temporary solution to fix python-mode completion suspend for long time.
  ;; `completion-at-point-functions': (python-completion-complete-at-point t)
  (dolist (hook '(python-mode-hook
                  inferior-python-mode-hook
                  ))
    (add-hook hook '(lambda ()
                      (setq-local completion-at-point-functions nil))))
  )


;;; [ elpy ] -- Emacs Python Development Environment.

(use-package elpy
  ;; :ensure t
  ;; :config
  ;; (setq elpy-rpc-backend 'rope)
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

;; (setq inferior-python-mode-hook '(python-shell-send-setup-code))
;; (setq inferior-python-mode-hook nil)

(add-hook 'inferior-python-mode-hook
          '(lambda ()
             (my-company-add-backends-to-mode '(company-anaconda))))


;;; [ pyenv-mode ]

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


;;; [ virtualenv ]


;;; [ virtualenvwrapper ]


;;; [ jedi.el ] --- a python auto-completion library.

;;; Usage:
;;
;; - (jedi:start-server)
;; - (jedi:stop-server)

;; (add-hook 'python-mode-hook 'jedi:setup)
;; ;; (setq jedi:complete-on-dot t)           ; optional, but you need `jedi:setup' instead of `jedi:ac-setup'.
;; ;; or
;; ;; use auto-complete and use jedi as ac source.
;; ;; (add-hook 'python-mode-hook 'jedi:ac-setup)
;;
;;
;; ;; variable (jedi:server-command '("~/.emacs.d/.python-environments/default/bin/jediepcserver.py"))
;; ;;    Command used to run Jedi server.
;; ;;    Note
;; ;;    If you used jedi:install-server (recommended) to install Python server jediepcserver.py, you don’t need to mess around with jediepcserver.py. Jedi.el handles everything automatically.
;; ;;
;; ;; install python jedi server (jediepcserver.py) by running:
;; ;; [M-x jedi:install-server] in Emacs.
;;
;; (setq jedi:tooltip-method '(pos-tip popup)) ; nil: show in minibuffer. '(pos-tip popup) : use tooltip.
;;
;; ;; (setq jedi:environment-root "jedi"
;; ;;       jedi:environment-virtualenv '("--python" "/PATH/TO/python3")
;; ;;       )


;;; [ company-jedi ]

;; (require 'jedi-core)

;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (jedi:start-server)
;;             ;; (company-jedi--setup)
;;             (my-company-add-backends-to-mode
;;              '(company-jedi
;;                ;; company-ropemacs
;;                ))
;;             ))

;; (setq jedi:install-server--command '("pip" "install" "--upgrade" "/home/stardiviner/.emacs.d/el-get/jedi/" "--no-cache-dir"))


;;; [ anaconda-mode ]

(use-package anaconda-mode
  :ensure t
  :config
  
  ;; show object docstring in eldoc.
  (defun anaconda-mode-docstring-eldoc-function ()
    (anaconda-mode-call "goto_definitions" 'anaconda-mode-docstring-eldoc-callback))

  (defun anaconda-mode-docstring-eldoc-callback (result)
    (eldoc-message
     (s-join " " (--map
                  (s-collapse-whitespace (cdr (assoc 'docstring it))) result))))

  (defun docstring-eldoc-init ()
    (make-local-variable 'eldoc-documentation-function)
    (setq-local eldoc-documentation-function
                'anaconda-mode-docstring-eldoc-function))

  (add-hook 'python-mode-hook 'docstring-eldoc-init)

  ;; enable anaconda-mode in python-mode.
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'eldoc-mode)
  )


;;; [ company-anaconda ]

(use-package company-anaconda
  :ensure t
  :config
  (add-hook 'python-mode-hook
            '(lambda ()
               (my-company-add-backends-to-mode '(company-anaconda))))
  )



(add-hook 'python-mode-hook
          (lambda ()
            ;; doc
            (cond
             ((featurep 'anaconda-mode)
              (local-set-key (kbd "C-h d d") 'anaconda-mode-show-doc)
              (local-set-key (kbd "M-.") 'anaconda-mode-goto))
             ((functionp 'jedi:show-doc)
              (local-set-key (kbd "C-h d d") 'jedi:show-doc)
              (local-set-key (kbd "M-.") 'jedi:goto-definition))
             (t
              (local-set-key (kbd "d") 'pydoc)
              (local-set-key (kbd "M-.") 'xref-find-definitions))
             )
            ;; inferior-python
            (define-key python-mode-map (kbd "C-c C-s") 'run-python)
            ))


;;; [ IPython ]



;;; [ ein ] -- IPython notebook client in Emacs




(provide 'init-my-prog-lang-python)

;;; init-my-prog-lang-python.el ends here
