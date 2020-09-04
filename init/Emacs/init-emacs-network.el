;;; init-emacs-network.el --- init for Emacs network.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ network functions ]

(require 'net-utils)

;;; [ Network Security Manager (NSM) ]

;; - M-x customize `network-security'
;; - `network-security-level'

;;; [ Proxy ]

;; (setq url-proxy-services
;;       '(;; Privoxy
;;         ("http"  . "127.0.0.1:8118")
;;         ("https" . "127.0.0.1:8118")
;;         ;; ("ftp"   . "b.qypac.net:57008")
;;         ;; don't proxy for localhost, avoid robe server (For Ruby) can't response.
;;         ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")
;;         ;; no proxy for baidu.com, sina.com etc. proxy for all others.
;;         ;; ("no_proxy" . "^.*\\(baidu\\|sina)\\.com")
;;         ))
;;
;; (setq url-using-proxy "http://127.0.0.1:8118")

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

;;; [ use-proxy ] -- Easy way to enable/disable proxies in Emacs and Elisp.

(use-package use-proxy
  :ensure t
  :commands (use-proxy-mode
             use-proxy-toggle-proxies-global
             use-proxy-toggle-proto-proxy
             use-proxy-with-custom-proxies
             use-proxy-with-specified-proxies)
  :custom ((use-proxy-http-proxy "localhost:8118")
           (use-proxy-https-proxy "localhost:8118")
           (use-proxy-no-proxy (regexp-opt '("localhost" "baidu.com")))))


(provide 'init-emacs-network)

;;; init-emacs-network.el ends here
