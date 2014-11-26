;;; init-prog-lang-shell.el --- init for Shell Sctips
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:




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
(add-hook 'find-file-hook 'insert-shebang)

(setq insert-shebang-env-path "/usr/bin/env"
      insert-shebang-file-types '(("sh" . "bash")
                                  ("rb" . "ruby") ("py" . "python") ("pl" . "perl")
                                  ("c" . "C") ("cpp" . "C++"))
      insert-shebang-custom-headers '(("c" . "#include <stdio.h>"))
      insert-shebang-ignore-extensions '("txt" "org" "markdown" "md")
      insert-shebang-track-ignored-filename "~/.insert-shebang.log"
      ;; insert-shebang-header-scan-limit 6
      )



(provide 'init-prog-lang-shell)

;;; init-prog-lang-shell.el ends here
