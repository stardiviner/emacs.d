;;; init-prog-lang-shell.el --- init for Shell Sctips
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


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



(provide 'init-prog-lang-shell)

;;; init-prog-lang-shell.el ends here
