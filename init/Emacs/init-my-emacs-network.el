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

;;; [ url-gw (url-gateway) ]

(require 'url-gw)
(require 'socks)

;; (setq url-gateway-method 'socks
;;       socks-noproxy '("localhost")
;;       socks-server '("Default server" "127.0.0.1" 1086 5))

;;; -------------------

;; (defun proxy-toggle ()
;;   (interactive)
;;   (if (getenv "HTTP_PROXY") ; TODO:
;;       (progn
;;         (setenv "HTTP_PROXY"  nil)
;;         (setenv "HTTPS_PROXY" nil)
;;         )
;;     ;; HTTP Proxy
;;     ;; Privoxy
;;     (setenv "HTTP_PROXY"  "http://localhost:8118")
;;     (setenv "HTTPS_PROXY" "http://localhost:8118")
;;     ;; socks v5 proxy
;;     (setq url-gateway-method 'socks
;;           socks-noproxy '("localhost")
;;           socks-server '("Default server" "127.0.0.1" 1080 5))
;;     )
;;   )


(provide 'init-my-emacs-network)

;;; init-my-emacs-network.el ends here
