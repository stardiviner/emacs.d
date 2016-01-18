;;; init-my-prog-web-browser.el --- init for Web Browser
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ dom ]


;;; [ kite ] -- WebKit


;;; [ skewer-mode ] -- Live web development in Emacs.

;;; Usage:
;;
;; 1. M-x run-skewer to attach a browser to Emacs.
;;
;; 2. From a js2-mode buffer with skewer-mode minor mode enabled, send forms to
;;    the browser to evaluate
;;
;; - [C-x C-e] :: Evaluate the form before the point and display the result in
;;   the minibuffer. If given a prefix argument, insert the result into the
;;   current buffer.
;; - [C-M-x] :: Evaluate the top-level form around the point.
;; - [C-c C-k] :: Load the current buffer.
;; - [C-c C-z] :: Select the REPL buffer.
;;
;; - CSS
;;
;;   - [C-x C-e] :: Load the declaration at the point.
;;   - [C-M-x] :: Load the entire rule around the point.
;;   - [C-c C-k] :: Load the current buffer as a stylesheet.
;;
;; - HTML
;;
;;   - [C-M-x] :: Load the HTML tag immediately around the point.
;;
;; - REPL
;;
;;   - [M-x skewer-repl] / [C-c C-z] :: A REPL into the browser.
;;     Messages can be logged to this REPL with skewer.log() (like console.log()).

(use-package skewer-mode
  :ensure t
  :config
  ;; (skewer-setup)
  ;; or
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'js3-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode)
  )



;;; [ livid-mode ] -- Live browser eval of JavaScript every time a buffer changes.

;; 1. [M-x livid-mode] on js2-mode.
;; 2. make sure simple-httpd server running.
;; 3. <script src="http://localhost:8080/skewer"></script>

(use-package livid-mode
  :ensure t
  )


(provide 'init-my-prog-web-browser)

;;; init-my-prog-web-browser.el ends here
