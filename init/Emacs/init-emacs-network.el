;;; init-emacs-network.el --- init for Emacs network.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Network Security Manager (NSM) ]

;; - M-x customize `network-security'
;; - `network-security-level'

;;; [ Proxy ]

;; (setq url-proxy-services
;;       '(("http"  . "http://b.qypac.net:57008")
;;         ("https" . "127.0.0.1:1080")
;;         ("ftp"   . "b.qypac.net:57008")
;;         ;; don't use `localhost', avoid robe server (For Ruby) can't response.
;;         ("no_proxy" . "127.0.0.1")
;;         ("no_proxy" . "^.*\\(baidu\\|sina)\\.com")
;;         ))

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

;;; [ proxy-mode ] A global minor mode to toggle proxy inside of Emacs.

(use-package proxy-mode
  :ensure t
  :commands (proxy-mode)
  :init (setq proxy-mode-socks-proxy '("Default server" "127.0.0.1" 1086 5)))


(provide 'init-emacs-network)

;;; init-emacs-network.el ends here
