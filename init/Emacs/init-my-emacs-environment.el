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
;; [C-h v current-language-environment]
(set-default-coding-systems 'utf-8)
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-language-environment 'utf-8)       ; Original "English"

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(prefer-coding-system 'utf-8-unix)

;; (setq-default buffer-file-coding-system 'utf-8-unix)
;; (setq-default default-buffer-file-coding-system 'utf-8-unix)


;;; Proxy

;; (setenv "no_proxy" "127.0.0.1,localhost"
;; 	"http_proxy" "")

(setq url-proxy-services
      '(("no_proxy" . "127.0.0.1") ; don't use `localhost', avoid robe server (For Ruby) can't response.
        ("http" . "112.124.9.128:44433")
        ;; ("https" . "")
        ))

;;; AUTHORITY
;; (setq url-http-proxy-basic-auth-storage
;;       (list (list "proxy.com:8080"
;; 		  (cons "Input your LDAP UID !"
;; 			(base64-encode-string "LOGIN:PASSWORD")))))


(provide 'init-my-emacs-environment)

;;; init-my-emacs-environment.el ends here
