;;; init-emacs-theme.el --- Init for Themes
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ X Resources ]

(setq inhibit-x-resources t)

;;; [ font lock ]

(global-font-lock-mode t)

(require 'color) ; for `color-lighten-name' and `color-darken-name'

(load-theme 'adwaita)

;;; [ leuven-theme ]

;; (use-package leuven-theme
;;   :load-path "~/Code/Emacs/leuven-theme/"
;;   :no-require t
;;   :init (require 'leuven-theme)
;;   :config (load-theme 'leuven t))

;;; [ one-themes ] -- One color scheme.

(leaf one-themes
  :doc "My customized one-themes custom theme."
  ;; :straight (one-themes :type git :host github :repo "stardiviner/one-thems")
  :load-path "~/Code/Emacs/one-themes"
  :require t
  :config (load-theme 'one-dark t))



(provide 'init-emacs-theme)

;;; init-emacs-theme.el ends here
