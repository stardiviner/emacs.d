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





(provide 'init-my-prog-lang-python)

;;; init-my-prog-lang-python.el ends here
