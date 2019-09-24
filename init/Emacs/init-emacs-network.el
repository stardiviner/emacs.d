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
;;       '(;; shadowsocks
;;         ("http"  . "127.0.0.1:1086")
;;         ("https" . "127.0.0.1:1086")
;;         ;; ("ftp"   . "b.qypac.net:57008")
;;         ;; don't proxy for localhost, avoid robe server (For Ruby) can't response.
;;         ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")
;;         ;; no proxy for baidu.com, sina.com etc. proxy for all others.
;;         ;; ("no_proxy" . "^.*\\(baidu\\|sina)\\.com")
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

;; (use-package proxy-mode
;;   :ensure t
;;   :defer t
;;   :init (setq proxy-mode-socks-proxy '("Default server" "127.0.0.1" 1086 5)))

;;; [ with-proxy ] -- Evaluate expressions within Proxy.

(use-package with-proxy
  :ensure t
  :defer t
  :commands (with-proxy with-proxy-url with-proxy-shell))


(provide 'init-emacs-network)

;;; init-emacs-network.el ends here
