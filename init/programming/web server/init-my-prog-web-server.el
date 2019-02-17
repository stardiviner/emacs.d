;;; init-prog-web-server.el --- init for Web Server
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'web-server-prefix)
  (define-prefix-command 'web-server-prefix))
(define-key tools-prefix (kbd "w") 'web-server-prefix)

;;; [ elnode ] -- Asynchronous (non-blocking evented IO) HttpServer framework written in Emacs Lisp bind to Node.js.

;;; Usage:
;;
;; 1. [M-x elnode-make-webserver]
;; 2. Serve files from: [enter directory]
;; 3. TCP Port: (try something over 8000): 8009
;;
;; - `elnode-make-webserver'
;; - `elnode-start' & `elnode-stop'
;; - `list-elnode-servers' & `elnode-server-list'

(use-package elnode
  :ensure t
  :defer t
  :commands (elnode-make-webserver elnode-server-list elnode-start elnode-stop)
  :bind (:map web-server-prefix ("e" . elnode-make-webserver))
  :init (setq elnode-init-host "localhost"
              elnode-init-port "8000"))

;;; [ web-server ]

(use-package web-server
  :ensure t
  :defer t)

;;; [ websocket ] -- Emacs WebSocket Client and Server

(use-package websocket
  :ensure t
  :defer t)


(provide 'init-prog-web-server)

;;; init-prog-web-server.el ends here
