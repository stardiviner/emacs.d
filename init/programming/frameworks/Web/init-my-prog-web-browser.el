;;; init-my-prog-web-browser.el --- init for Web Browser
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ dom ]


;;; [ kite ] -- WebKit


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


(provide 'init-my-prog-web-browser)

;;; init-my-prog-web-browser.el ends here
