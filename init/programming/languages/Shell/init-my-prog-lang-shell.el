;;; init-my-prog-lang-shell.el --- init for Shell Scripts
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Shell Script (`sh-mode') ]

(setq sh-indentation 4)

;; (setq sh-dynamic-complete-functions '(shell-environment-variable-completion
;;                                       shell-command-completion
;;                                       comint-filename-completion)
;;       shell-dynamic-complete-functions '(comint-c-a-p-replace-by-expanded-history
;;                                          shell-environment-variable-completion
;;                                          shell-command-completion
;;                                          shell-c-a-p-replace-by-expanded-directory
;;                                          pcomplete-completions-at-point
;;                                          shell-filename-completion
;;                                          comint-filename-completion)
;;       )
;;
;; (add-hook 'sh-mode-hook
;;           '(lambda ()
;;              (remove-hook 'completion-at-point-functions
;;                           'sh-completion-at-point-function)
;;              ))


;;; [ Bash ]



;;; [ Zsh ]
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

;; A programmatic way of selecting a flavor when you don't want to use the
;; shebang is doing this in a sh-mode buffer:
;;
;; (sh-set-shell "zsh")
;;
(add-hook 'sh-mode-hook
          (lambda ()
            (if (string-match "\\.zsh$" buffer-file-name)
                (sh-set-shell "zsh"))))


;;; [ insert-shebang ]

(require 'insert-shebang)

;; enable it globally
;; (add-hook 'find-file-hook 'insert-shebang)
;; only enable it on some modes
(dolist (hook '(sh-mode-hook
                ))
  (add-hook hook 'insert-shebang))

(setq insert-shebang-env-path "/usr/bin/env"
      insert-shebang-file-types '(("sh" . "bash"))
      insert-shebang-custom-headers '(("c" . "#include <stdio.h>")
                                      ("R" . "#!/usr/bin/env r"))
      ;; insert-shebang-ignore-extensions '("txt" "org" "markdown" "md")
      ;; insert-shebang-track-ignored-filename "~/.insert-shebang.log"
      ;; insert-shebang-header-scan-limit 6
      )


;;; disable flycheck in sh-mode to fix suspend in shell script source code file.
;; TODO: test whether the problem is on `flycheck'. or on `company-backends'.
(add-hook 'sh-mode-hook
          (lambda ()
            (flycheck-mode -1)))


(provide 'init-my-prog-lang-shell)

;;; init-my-prog-lang-shell.el ends here
