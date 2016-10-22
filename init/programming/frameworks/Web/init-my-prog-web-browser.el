;;; init-my-prog-web-browser.el --- init for Web Browser
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ dom ]


;;; [ kite ] -- Emacs front end for the WebKit Inspector.


;;; [ kite-mini ] -- Yet another Emacs package to interact with WebKit remote debugging API.

(use-package kite-mini
  :ensure t
  :defer t
  :config
  ;; Automatically Turn on the mode for your buffer of choice.
  (add-hook 'js-mode-hook (lambda () (kite-mini-mode t)))
  (add-hook 'css-mode-hook (lambda () (kite-mini-mode t)))
  )


(provide 'init-my-prog-web-browser)

;;; init-my-prog-web-browser.el ends here
