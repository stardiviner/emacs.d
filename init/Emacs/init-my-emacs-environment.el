;;; init-my-emacs-environment.el --- init Emacs environment variables

;;; Commentary:

;;; Code:

;;; [ User Information ]

(setq user-full-name "stardiviner")
(setq user-mail-address "numbchild@gmail.com")
;; (setq user-login-name "stardiviner")


;;; $PATH
;;; way 1:
;;     (setq exec-path (append exex-path '("~/bin")))
;;; way 2:
;; (defun eshell-mode-hook-func ()
;;   (setq eshell-path-env (concat "~/bin:" eshell-path-env))
;;   (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH"))))
;; (add-hook 'eshell-mode-hook 'eshell-mode-hook-func)


;;; Systems

;; TODO: add to this init file.
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/System-Environment.html


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


;;; Proxy

;; (setq url-proxy-services
;;       '(
;;         ;; ("http"  . "http://b.qypac.net:57008")
;;         ;; ("https" . "http://b.qypac.net:57008")
;;         ;; ("ftp"   . "b.qypac.net:57008")
;;         ;; don't use `localhost', avoid robe server (For Ruby) can't response.
;;         ;; ("no_proxy" . "127.0.0.1")
;;         ;; ("no_proxy" . "^.*\\(baidu\\|sina)\\.com")
;;         ))

;; (setq url-using-proxy "http://b.qypac.net:57008")

;;; AUTHORITY
;; (setq url-http-proxy-basic-auth-storage
;;       (list (list "proxy.com:8080"
;; 		  (cons "Input your LDAP UID !"
;; 			(base64-encode-string "LOGIN:PASSWORD")))))

;; (setenv "no_proxy" "127.0.0.1,localhost"
;; 	"http_proxy" "")

(defun proxy-toggle ()
  (interactive)
  (if (getenv "HTTP_PROXY")
      (progn
        (setenv "HTTP_PROXY"  nil)
        (setenv "HTTPS_PROXY" nil)
        )
    (setenv "HTTP_PROXY"  "http://b.qypac.net:57008")
    (setenv "HTTPS_PROXY" "http://b.qypac.net:57008")
    )
  )


(provide 'init-my-emacs-environment)

;;; init-my-emacs-environment.el ends here
