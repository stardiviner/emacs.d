;;; init-my-prog-nginx.el --- init Nginx mode
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ nginx-mode ]

;; https://github.com/ajc/nginx-mode
;; https://github.com/zev/nginx-mode

(require 'nginx-mode)

(autoload 'nginx-mode "nginx-mode")
;; (require 'nginx-mode)

(add-to-list 'auto-mode-alist
             '("nginx\\.conf$" . nginx-mode)
             '("/etc/nginx/.*" . nginx-mode))



(provide 'init-my-prog-nginx)

;;; init-my-prog-nginx.el ends here
