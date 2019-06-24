;;; init-emacs-theme.el --- Init for Themes
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ X Resources ]

(setq inhibit-x-resources t)

;;; [ font lock ]

(global-font-lock-mode t)

(require 'color) ; for `color-lighten-name' and `color-darken-name'

;; (use-package delight ; for `doom-themes'
;;   :ensure t
;;   :delight buffer-face-mode)

;;; [ leuven-theme ]

;; (use-package leuven-theme
;;   :load-path "~/Code/Emacs/leuven-theme/"
;;   :no-require t
;;   :init (require 'leuven-theme)
;;   :config (load-theme 'leuven t))

;; (use-package parchment-theme
;;   :ensure t
;;   :config (load-theme 'parchment t))

;;; [ eziam-theme ] -- A mostly monochrome theme, inspired by Tao and Leuven, with dark and light versions.

;; (use-package eziam-theme
;;   :ensure t
;;   :no-require t
;;   :load (eziam-light-theme))

;;; [ spacemacs-theme ] -- Spacemacs default color-theme.

;; (use-package spacemacs-theme
;;   :ensure t
;;   :no-require t
;;   :config (load-theme 'spacemacs-dark t)
;;   (set-face-attribute 'mode-line nil
;;                       :background (color-darken-name (face-background 'default) 5)
;;                       :box '(:color "DarkSlateBlue" :line-width 2))
;;   (with-eval-after-load 'ivy
;;     (set-face-attribute 'ivy-current-match nil
;;                         :background "SeaGreen" :foreground "white"
;;                         :weight 'normal)))

;;; [ one-themes ] -- One color scheme.

(use-package one-themes
  ;; :ensure t
  ;; :quelpa (one-themes :fetcher git :repo "stardiviner/emacs-one-themes")
  :load-path "~/Code/Emacs/one-themes"
  :config (load-theme 'one-dark t))

;;; [ dimmer ] -- Interactively highlight which buffer is active by dimming the others.

;; (use-package dimmer
;;   :ensure t
;;   :init (setq dimmer-exclusion-regexp "*company-*")
;;   :config (dimmer-mode 1))


(provide 'init-emacs-theme)

;;; init-emacs-theme.el ends here
