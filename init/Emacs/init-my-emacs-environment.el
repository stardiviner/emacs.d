;;; init-my-emacs-environment.el --- init Emacs environment variables

;;; Commentary:

;;; Code:

;;; [ User Information ]

(setq user-full-name "stardiviner")
(setq user-mail-address "numbchild@gmail.com")
;; (setq user-login-name "stardiviner")


;;; $PATH
;;; way 0:
;; (setenv "PATH" (concat (getenv "PATH") ":~/bin"))
;;; way 1:
;;     (setq exec-path (append exex-path '("~/bin")))
;;; way 2:
;; (defun eshell-mode-hook-func ()
;;   (setq eshell-path-env (concat "~/bin:" eshell-path-env))
;;   (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH"))))
;; (add-hook 'eshell-mode-hook 'eshell-mode-hook-func)


;;; Systems


;; format (Unix, DOS) & encoding

;; [C-h v current-language-environment]

(set-default-coding-systems 'utf-8)
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-language-environment 'utf-8)       ; Original "English"

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(prefer-coding-system 'utf-8-unix)
(set-charset-priority 'unicode)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; (setq-default buffer-file-coding-system 'utf-8-unix)
;; (setq-default default-buffer-file-coding-system 'utf-8-unix)


;;; time

(setq system-time-locale "C") ; make timestamps in org-mode appear in English.



(provide 'init-my-emacs-environment)

;;; init-my-emacs-environment.el ends here
