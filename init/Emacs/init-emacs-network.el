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


;;; ----------------------------------------------------------
(defvar my:proxy-toggle-p nil
  "A global variable indicate current proxy toggle status.
Used by function `my:proxy-toggle'.")

(defun my:proxy-toggle (proxy)
  "A command to toggle `PROXY' for Emacs."
  (interactive (list (unless my:proxy-toggle-p
                       (completing-read "Select a proxy routine: " '("socks" "url_proxy_services" "env HTTP_PROXY")))))
  (if my:proxy-toggle-p
      (setq my:proxy-toggle-p nil)
    (setq my:proxy-toggle-p proxy))
  (pcase my:proxy-toggle-p
    ("socks"
     (setq url-gateway-method 'socks
           socks-noproxy '("localhost")
           socks-server '("Default server" "127.0.0.1" 1086 5)))
    ("url_proxy_services"
     (setq url-proxy-services
           '(("http"  . "127.0.0.1:8118")
             ("https" . "127.0.0.1:8118")
             ("ftp"   . "127.0.0.1:8118")
             ;; don't use `localhost', avoid robe server (For Ruby) can't response.
             ("no_proxy" . "127.0.0.1")
             ("no_proxy" . "^.*\\(baidu\\|sina)\\.com")
             )))
    ("env HTTP_PROXY"
     ;; Privoxy
     (setenv "HTTP_PROXY"  "http://localhost:8118")
     (setenv "HTTPS_PROXY" "http://localhost:8118"))
    (_
     (setq url-gateway-method 'native)
     (setq url-proxy-services nil)
     (setenv "HTTP_PROXY"  nil)
     (setenv "HTTPS_PROXY" nil))
    ))




(provide 'init-emacs-network)

;;; init-emacs-network.el ends here
