;;; init-my-emacs-environment.el --- init Emacs environment variables

;;; Commentary:

;;; Code:


;;; $PATH
;;; way 1:
;;     (setq exec-path (append exex-path '("~/bin")))
;;; way 2:
;; (defun eshell-mode-hook-func ()
;;   (setq eshell-path-env (concat "~/bin:" eshell-path-env))
;;   (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH"))))
;; (add-hook 'eshell-mode-hook 'eshell-mode-hook-func)


;; format (Unix, DOS) & encoding
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)

(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)






(provide 'init-my-emacs-environment)

;;; init-my-emacs-environment.el ends here
