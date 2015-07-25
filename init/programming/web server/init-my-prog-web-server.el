;;; init-my-prog-web-server.el --- init for Web Server
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(unless (boundp 'my-web-server-prefix)
  (define-prefix-command 'my-web-server-prefix))
(define-key my-tools-prefix-map (kbd "w") 'my-web-server-prefix)


;;; [ elnode ] -- Asynchronous (non-blocking evented IO) HttpServer framework written in Emacs Lisp.

;;; Usage:
;;
;; 1. [M-x elnode-make-webserver]
;; 2. Serve files from: [enter directory]
;; 3. TCP Port: (try something over 8000): 8009
;;
;; - `elnode-make-webserver'
;; - `elnode-start' & `elnode-stop'
;; - `list-elnode-servers' & `elnode-server-list'

(require 'elnode)

(setq elnode-init-host "localhost"
      elnode-init-port "8000"
      )

(define-key my-web-server-prefix (kbd "e") 'elnode-make-webserver)


(provide 'init-my-prog-web-server)

;;; init-my-prog-web-server.el ends here
