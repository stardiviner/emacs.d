;;; init-my-prog-lang-javascript.el --- init JavaScript for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:

;; http://www.emacswiki.org/emacs/JavaScript

;;; Code:

;;; [ JavaScript ]

(add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))


;;; [ javascript-mode (js-mode) ]

(eval-after-load 'js-mode
  '(progn
     (add-hook 'js-mode-hook
               (lambda ()
                 (electric-layout-mode -1) ; electric-layout-mode doesn't play nice with js-mode.
                 ))))


;;; [ js2-mode ]

;; (autoload 'js2-mode "js2-mode" nil t)
;;
;; (eval-after-load 'auto-complete
;;   (add-to-list 'ac-modes 'js2-mode))


;;; [ js3-mode ]

(require 'js3-mode)

;; (eval-after-load 'auto-complete
;;   (add-to-list 'ac-modes 'js3-mode))


;;; JavaScript subprocess integration


;;; [ SwankJS ]

;; a full-blown SlimeMode backend for node.js and can connect to a running web browser process through the socket.io package.
;; https://github.com/swank-js/swank-js

(define-key js3-mode-map [f5] 'slime-js-reload)

(add-hook 'js3-mode-hook
          (lambda ()
            (slime-js-minor-mode 1)))

(add-hook 'css-mode-hook
          (lambda ()
            (define-key css-mode-map (kbd "C-M-x") 'slime-js-refresh-css)
            (define-key css-mode-map (kbd "C-c C-r") 'slime-js-embed-css)))



(provide 'init-my-prog-lang-javascript)

;;; init-my-prog-lang-javascript.el ends here
