;;; init-my-prog-lang-python.el --- init Python Programming Language Environment
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ python-mode ] --- Python's flying circus support for Emacs

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


;;; [ pydoc ]

;; (eval-after-load 'python
;;   '(if (featurep 'helm)
;;        (define-key my-prog-help-document-map (kbd "d") 'helm-pydoc)
;;      (define-key my-prog-help-document-map (kbd "d") 'pydoc)))


;;; [ Inferior Python ]

;; - [C-c C-l] :: `python-shell-send-file'
;; - [C-c C-r] :: `python-shell-send-region'
;; - [C-c C-c] :: `python-shell-send-buffer'
;; - [C-M-x]   :: `python-shell-send-defun'
;; - `python-shell-send-string'

(defun inferior-python ()
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
(define-key my-prog-inferior-map (kbd "p") 'inferior-python)


;; (setq inferior-python-mode-hook '(python-shell-send-setup-code))
;; (setq inferior-python-mode-hook nil)

(add-hook 'inferior-python-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends)
                         'company-anaconda)))


;;; [ pyvenv ] -- Python virtual environment interface for Emacs.

;;; This is a simple global minor mode which will replicate the changes done by
;;; virtualenv activation inside Emacs.

;;; Usage:
;;
;; - `pyvenv-activate' :: queries the user for a virtual environment directory to activate.
;; - `pyvenv-workon' :: queries for a virtual environment in $WORKON_HOME (from virtualenvwrapper.sh).
;; - `pyvenv-mode-line-indicator' :: an indicator for mode-line.

;; TODO: config it.


;;; [ pyenv-mode ]


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
;;             (add-to-list (make-local-variable 'company-backends)
;;                          '(company-jedi
;;                            ;; company-ropemacs
;;                            ))
;;             ))

;; (setq jedi:install-server--command '("pip" "install" "--upgrade" "/home/stardiviner/.emacs.d/el-get/jedi/" "--no-cache-dir"))


;;; [ anaconda-mode ]

;;; Usage:
;;
;; - context-sensitive code completion
;; - jump to definitions
;; - find references
;; - view documentation
;; - virtual environment
;; - eldoc mode

;;; Keybindings
;;
;; - [M-.] :: anaconda-mode-goto-definitions
;; - [M-*] :: anaconda-nav-pop-marker
;; - [M-?] :: anaconda-mode-view-doc
;; - [M-r] :: anaconda-mode-usages

;;; Implementation details
;;
;; Anaconda mode comes with anaconda_mode.py server. This server allow you to
;; use jedi python library over jsonrpc api. Server choice first available port
;; starting from 24970. Anaconda mode will run this server automatically on
;; first call of any anaconda-mode command.
;;
;; This mean that completion results and reference search depends on your
;; project installation. To make it available for anaconda-mode you have few
;; options.

(use-package anaconda-mode
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'eldoc-mode)
  )


;;; [ company-anaconda ]

(add-hook 'python-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends)
                         'company-anaconda)))



(add-hook 'python-mode-hook
          (lambda ()
            ;; doc
            (cond
             ((featurep 'anaconda-mode)
              (local-set-key (kbd "C-h d d") 'anaconda-mode-show-doc)
              (local-set-key (kbd "M-.") 'anaconda-mode-goto))
             ((featurep 'helm)
              (local-set-key (kbd "C-h d d") 'helm-pydoc))
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


;;; [ virtualenv ]


;;; [ virtualenvwrapper ]


;;; [ IPython ]



;;; [ helm-ipython ]

;; (require 'helm-ipython)


;;; [ ein ] -- IPython notebook client in Emacs




(provide 'init-my-prog-lang-python)

;;; init-my-prog-lang-python.el ends here
