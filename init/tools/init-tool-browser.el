;;; init-tool-browser.el --- init for Browser
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; global keybindings

(unless (boundp 'browser-prefix)
  (define-prefix-command 'browser-prefix))
(define-key tools-prefix (kbd "b") 'browser-prefix)


;;; [ browse-url ] -- default browser function

;; system default browser: (`browser-url-browser-function')
;; - `browse-url-generic'
;; - `browse-url-default-browser'
;; - `browse-url-chrome'
;; - `browse-url-firefox'
;; - `browse-url-conkeror'
;; - `eww-browse-url' (EWW)
;; - `xwidget-webkit-browse-url'

;;; set default browser to generic browser
;; (setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program (executable-find "firefox"))
;;
;;; set default browser to "Firefox"
(setq browse-url-browser-function 'browse-url-firefox)
(setq browse-url-firefox-program (executable-find "firefox"))
;;
;;; set default browser to "Google Chrome"
;; (setq browse-url-browser-function 'browse-url-chrome)
;; (setq browse-url-chrome-program (executable-find "google-chrome-unstable"))

(require 'init-eww)
(require 'init-w3m)

;;; [ xwidget-webkit ]
(define-key browser-prefix (kbd "C-b") 'xwidget-webkit-browse-url)

;;; global keybindings

(if (featurep 'eww)
    (progn
      (define-key browser-prefix (kbd "b") 'eww)
      (define-key browser-prefix (kbd "o") 'eww-follow-link)
      )
  (progn
    (define-key browser-prefix (kbd "o") 'browse-url-at-point)
    (define-key browser-prefix (kbd "g") 'w3m-goto-url)
    (define-key browser-prefix (kbd "s") 'w3m-search)
    )
  )


;;; [ ace-link ] -- easier link selection with ace-mode on many buffer links.

(use-package ace-link
  :ensure t
  :defer t
  :init (ace-link-setup-default))



(provide 'init-tool-browser)

;;; init-tool-browser.el ends here
