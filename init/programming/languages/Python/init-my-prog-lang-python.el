;;; init-my-prog-lang-python.el --- init Python Programming Language Environment
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ python ] --- Python's flying circus support for Emacs

;;  Introduction
;;
;; This is now the official Python major mode for GNU Emacs.
;;
;; It aims to provide the stuff you'll expect from a major mode for python editing while keeping it simple.
;;
;; Currently it implements Syntax highlighting, Indentation, Movement, Shell
;; interaction, Shell completion, Shell virtualenv support, Pdb tracking, Symbol
;; completion, Skeletons, FFAP, Code Check, Eldoc, Imenu.
;;
;;     Syntax highlighting
;;     Solid (auto)indentation support
;;     auto-detection of indentation levels for current file
;;     Robust triple quoted strings support
;;     Fancy variable assignment colorization
;;     Movement commands you'll expect from a major-mode.
;;     Sexp-like movement
;;     Python shell integration (not only for Python 2 but also Python 3!)
;;     Python shell completion (Same as above!)
;;     Python shell virtualenv support (as simple as setting a variable!)
;;     PDB Tracking (it even supports ipdb!)
;;     Symbol completion that sucks because a running inferior shell process and valid code in the current buffer are needed (Don't blame me, it's like that in every python-mode I know). Notice I don't recommend this thing, use ropemacs instead
;;     Skeletons with a tight integration with dabbrev out of the box
;;     FFAP (Find Filename At Point), click on an import statement and go to the module definition.
;;     Code check via pychecker by default (this is customizable of course)
;;     Eldoc support (this suffers the same drawbacks as the symbol completion, but it's the only sane way to do it from Elisp)
;;     imenu support to easily navigate your code
;;     add-log-current-defun support
;;     hideshow support
;;     outline support
;;     fill paragraph (with customizable docstring formatting)
;;
;; The code is well organized in parts with some clean sensitive naming.

(require 'python)

(setq-default python-indent-offset 4
              python-indent-guess-indent-offset t
              ;; python-shell-interpreter "python" ; "python", "ipython", "bpython"
              ;; python-shell-interpreter-args "-i"
              ;; python-shell-interpreter-interactive-arg "-i"
              ;; python-shell-prompt-regexp ">>> "
              ;; python-shell-prompt-block-regexp "\\.\\.\\. "
              ;; python-shell-prompt-output-regexp ""
              ;; python-shell-prompt-input-regexps '(">>> " "\\.\\.\\. " "In \\[[0-9]+\\]: " "   \\.\\.\\.: " "In : " "\\.\\.\\.: ")
              ;; python-shell-prompt-output-regexps '("" "Out\\[[0-9]+\\]: " "Out :")
              ;; python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
              ;; python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
              ;; python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
              )


;;; [ python-mode ]

;; (autoload 'python-mode "python" nil t)


;;; [ pydoc ]

;; (eval-after-load 'python
;;   '(if (featurep 'helm)
;;        (define-key my-prog-help-document-map (kbd "d") 'helm-pydoc)
;;      (define-key my-prog-help-document-map (kbd "d") 'pydoc)))


;;; [ Inferior Python ]

;; Usage:
;; (run-python)


;;; [ jedi.el ] --- a python auto-completion library.

;;; Usage:
;;
;; - (jedi:start-server)
;; - (jedi:stop-server)

;; (require 'jedi)
;;
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
;; (require 'company-jedi)

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


(require 'anaconda-mode)

;; virtualenv
;; (setq python-shell-virtualenv-root "~/.virtualenvs/python3/bin/virtualenv")

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)

;; (define-key anaconda-mode-map (kbd "M-.") 'anaconda-mode-goto)
;; ;; (define-key anaconda-mode-map (kbd "M-.") 'anaconda-mode-goto-definitions)
;; ;; (define-key anaconda-mode-map (kbd "M-,") 'anaconda-mode-goto-assignments)
;; (define-key anaconda-mode-map (kbd "M-?") 'anaconda-mode-view-doc)
;; (define-key anaconda-mode-map (kbd "M-r") 'anaconda-mode-usages)
;; (define-key anaconda-mode-map (kbd "M-*") 'anaconda-nav-pop-marker)


;;; [ company-anaconda ]

(require 'company-anaconda)

(add-hook 'python-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends)
                         'company-anaconda)))



(add-hook 'python-mode-hook
          (lambda ()
            (cond
             ((featurep 'anaconda-mode)
              (local-set-key (kbd "C-h d d") 'anaconda-mode-view-doc)
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
            ))


;;; [ IPython ]



;;; [ helm-ipython ]

;; (require 'helm-ipython)


;;; [ ein ] -- IPython notebook client in Emacs





(provide 'init-my-prog-lang-python)

;;; init-my-prog-lang-python.el ends here
