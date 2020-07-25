;;; init-emacs-environment.el --- init Emacs environment variables

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
(add-to-list 'exec-path (expand-file-name "~/bin"))
;;; way 2:
;; (defun eshell-mode-hook-func ()
;;   (setq eshell-path-env (concat "~/bin:" eshell-path-env))
;;   (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH"))))
;; (add-hook 'eshell-mode-hook 'eshell-mode-hook-func)


;;; Systems


;; format (Unix, DOS) & encoding

;; [C-h v current-language-environment]

;;; [ coding system ]

(set-default-coding-systems 'utf-8)

;;; auto guess file buffer encoding. (last is highest priority)
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Recognize-Coding.html#Recognize-Coding
(prefer-coding-system 'cp950)
(prefer-coding-system 'gb2312)
(prefer-coding-system 'cp936)
(prefer-coding-system 'gb18030)
(prefer-coding-system 'utf-16)
(prefer-coding-system 'utf-8-dos)
(prefer-coding-system 'utf-8-unix)

;; (set-charset-priority 'unicode)
;; (set-language-environment 'utf-8)
;; (set-file-name-coding-system 'utf-8)
;; (set-buffer-file-coding-system 'utf-8)
;; (set-selection-coding-system 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)

;; (setq locale-coding-system 'utf-8)
;; (setq buffer-file-coding-system 'utf-8-unix
;;       default-file-name-coding-system 'utf-8-unix
;;       default-keyboard-coding-system 'utf-8-unix
;;       default-process-coding-system '(utf-8-unix . utf-8-unix)
;;       default-sendmail-coding-system 'utf-8-unix
;;       default-terminal-coding-system 'utf-8-unix)

;;; open file with specified default encoding.
;; (add-to-list 'file-coding-system-alist '("\\.extension???" . utf-8))

;;; time

(setq system-time-locale "C") ; make timestamps in org-mode appear in English.



(provide 'init-emacs-environment)

;;; init-emacs-environment.el ends here
