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

(setq python-indent-offset 4
      python-indent-guess-indent-offset t
      ;; python-shell-interpreter "python" ; "ipython", "bpython"
      )


;;; [ python-mode ]

;; (require 'python-mode)

;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (setq python-indent-offset 4
;;                   python-shell-interpreter "ipython"
;;                   python-shell-interpreter-args ""
;;                   python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;;                   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
;;                   python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
;;                   python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
;;                   python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
;;                   )))


;;; [ pydoc ]

(if (featurep 'helm)
    (eval-after-load 'python-mode ; "python"
      '(progn
         (define-key pythom-mode-map (kbd "C-h d") 'helm-pydoc))))


;;; [ Inferior Python ]

;; Usage:
;; (run-python)


;;; [ jedi ] --- a python auto-completion library.

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



;;; [ IPython ]

;;; helm-ipython
;; (require 'helm-ipython)


;;; [ helm-ipython ]




;;; [ ein ] -- IPython notebook client in Emacs





(provide 'init-my-prog-lang-python)

;;; init-my-prog-lang-python.el ends here
