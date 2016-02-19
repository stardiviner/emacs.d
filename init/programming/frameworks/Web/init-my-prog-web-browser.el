;;; init-my-prog-web-browser.el --- init for Web Browser
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ dom ]


;;; [ swank-js ] -- Swank backend for Node.JS and in-browser JavaScript.

;; (use-package swank-js
;;   :ensure t
;;   :config
;;   (dolist (hook '(js2-mode-hook
;;                   js3-mode-hook
;;                   ))
;;     (add-hook hook #'slime-js-minor-mode))
;;
;;   (add-hook 'css-mode-hook
;;             (lambda ()
;;               (define-key css-mode-map (kbd "C-M-x") 'slime-js-refresh-css)
;;               (define-key css-mode-map (kbd "C-c C-r") 'slime-js-embed-css)))
;;
;;   (define-key js2-mode-map [f6] 'slime-js-reload)
;;   (define-key js3-mode-map [f6] 'slime-js-reload)
;;   )


;;; [ skewer-mode ] -- Live web development in Emacs.

;;; Usage:
;;
;; 1. =[M-x run-skewer]= to attach a browser to Emacs.
;;
;; 2. From a `js2-mode' buffer with `skewer-mode' *minor mode* enabled, send
;; forms to the browser to evaluate.

(use-package skewer-mode
  :ensure t
  :config
  (skewer-setup)
  )


;;; [ livid-mode ] -- Live browser eval of JavaScript every time a buffer changes.

;; 1. [M-x livid-mode] on js2-mode.
;; 2. make sure simple-httpd server running.
;; 3. <script src="http://localhost:8080/skewer"></script>

;; (use-package livid-mode
;;   :ensure t
;;   :config
;;   (add-hook 'js2-mode-hook 'livid-mode)
;;   )


;;; [ wooky ] -- Eval-defun for JavaScript in Chrome.


;;; [ kite ] -- Emacs front end for the WebKit Inspector.


;;; [ kite-minit ] -- Yet another Emacs package to interact with WebKit remote debugging API.

;;; Usage:
;;
;; 1. open Chrome with remote debugging enabled:
;;    $ chrome --remote-debugging-port=9222
;; 2. enable `kite-mini-mode' in `js-mode', or `css-mode'.

;; (use-package kite-mini
;;   :ensure t
;;   :config
;;   ;; Automatically Turn on the mode for your buffer of choice.
;;   (add-hook 'js-mode-hook (lambda () (kite-mini-mode t)))
;;   (add-hook 'css-mode-hook (lambda () (kite-mini-mode t)))
;;   )


(provide 'init-my-prog-web-browser)

;;; init-my-prog-web-browser.el ends here
