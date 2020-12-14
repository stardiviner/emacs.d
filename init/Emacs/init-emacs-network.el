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

;;; [ url-gw (url-gateway) ]

;; (require 'url-gw)
;; (require 'socks)
;;; NOTE: it only works for http: connections.
;; (setq url-gateway-method 'socks
;;       socks-noproxy '("localhost")
;;       socks-server '("Default server" "127.0.0.1" 1086 5))

;;; [ proxy-mode ] -- A minor mode to toggle proxy.

(use-package proxy-mode
  :ensure t
  :custom ((proxy-mode-emacs-socks-proxy '("Default server" "127.0.0.1" 1086 5))
           (proxy-mode-emacs-http-proxy
            '(("http"  . "127.0.0.1:8118") ; Privoxy
              ("https" . "127.0.0.1:8118")
              ;; NOTE: don't use `localhost', avoid local server like robe no response
              ;; ("no_proxy" . "127.0.0.1")
              )))
  :commands (proxy-mode))

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
