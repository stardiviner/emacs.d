;;; init-my-emacs-network.el --- init for Emacs network.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Network Security Manager (NSM) ]

;;; - M-x customize `network-security'
;;; - `network-security-level'

;;; [ Proxy ]

(setq url-proxy-services
      '(
        ;; ("http"  . "http://b.qypac.net:57008")
        ;; ("https" . "127.0.0.1:1080")
        ;; ("ftp"   . "b.qypac.net:57008")
        ;; don't use `localhost', avoid robe server (For Ruby) can't response.
        ;; ("no_proxy" . "127.0.0.1")
        ;; ("no_proxy" . "^.*\\(baidu\\|sina)\\.com")
        ))

;; (setq url-using-proxy "http://b.qypac.net:57008")

;;; AUTHORITY
;; (setq url-http-proxy-basic-auth-storage
;;       (list (list "proxy.com:8080"
;; 		  (cons "Input your LDAP UID !"
;; 			(base64-encode-string "LOGIN:PASSWORD")))))


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


(provide 'init-my-emacs-network)

;;; init-my-emacs-network.el ends here
