;;; init-my-prog-lang-python.el --- init Python Programming Language Environment
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ python-mode ]

(require 'python-mode)

(add-hook 'python-mode-hook
          (lambda ()
            (setq python-indent-offset 4
                  python-shell-interpreter "ipython"
                  python-shell-interpreter-args ""
                  python-shell-prompt-regexp "In \\[[0-9]+\\]: "
                  python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
                  python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
                  python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
                  python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
                  )))



;;; [ jedi ] --- a python auto-completion library.

(unless (package-installed-p 'jedi)
  (package-install 'jedi))
(require 'jedi)

;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)           ; optional, but you need `jedi:setup' instead of `jedi:ac-setup'.
;;
;; use auto-complete and use jedi as ac source.
(add-hook 'python-mode-hook 'jedi:ac-setup)


;; variable (jedi:server-command '("~/.emacs.d/.python-environments/default/bin/jediepcserver.py"))
;;    Command used to run Jedi server.
;;    Note
;;    If you used jedi:install-server (recommended) to install Python server jediepcserver.py, you don’t need to mess around with jediepcserver.py. Jedi.el handles everything automatically.
;;
;; install python jedi server (jediepcserver.py) by running:
;; [M-x jedi:install-server] in Emacs.

(setq jedi:tooltip-method '(pos-tip popup))

;; (setq jedi:environment-root "jedi"
;;       jedi:environment-virtualenv '("--python" "/PATH/TO/python3")
;;       )



(provide 'init-my-prog-lang-python)

;;; init-my-prog-lang-python.el ends here
