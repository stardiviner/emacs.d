;;; init-prog-web-browser.el --- init for Web Browser
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ dom ] -- XML/HTML (etc.) DOM manipulation and searching functions.

;; (use-package dom
;;   :defer t)

;;; [ kite ] -- Emacs front end for the WebKit Inspector.

;; (use-package kite
;;   :ensure t)

;;; [ kite-mini ] -- Yet another Emacs package to interact with WebKit remote debugging API.

;; (use-package kite-mini
;;   :ensure t
;;   :defer t
;;   :config
;;   ;; Automatically Turn on the mode for your buffer of choice.
;;   (add-hook 'js-mode-hook (lambda () (kite-mini-mode t)))
;;   (add-hook 'css-mode-hook (lambda () (kite-mini-mode t)))
;;   )


(provide 'init-prog-web-browser)

;;; init-prog-web-browser.el ends here
