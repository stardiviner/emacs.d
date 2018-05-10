;;; init-emacs-theme.el --- Init for Themes
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ X Resources ]

(setq inhibit-x-resources t)

;;; [ font lock ]

(global-font-lock-mode t)

(require 'color) ; for `color-lighten-name' and `color-darken-name'

;;; [ leuven-theme ]

(use-package leuven-theme
  :load-path "~/Code/Emacs/leuven-theme/"
  ;; :quelpa ((leuven-theme :fetcher github :repo "stardiviner/leuven-theme") :upgrade t)
  :config (load-theme 'leuven t))

;;; [ spacemacs-theme ] -- Spacemacs default color-theme.

;; (use-package spacemacs-theme
;;   :ensure t
;;   :no-require t
;;   :config
;;   (load-theme 'spacemacs-dark t))


(provide 'init-emacs-theme)

;;; init-emacs-theme.el ends here
