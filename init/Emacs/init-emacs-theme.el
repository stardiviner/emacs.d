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
  :no-require t
  :defer t
  :init (require 'leuven-theme)
  ;; :config (load-theme 'leuven t)
  )

;;; [ one-themes ] -- One color scheme.

(use-package one-themes
  :load-path "~/Code/Emacs/one-themes"
  :no-require t
  :defer t
  :init (require 'one-themes)
  ;; :config (load-theme 'one-dark t)
  )

(use-package circadian
  :ensure t
  :config
  (setq calendar-location-name "Shaoxing Town"
        calendar-time-zone +480
        calendar-latitude 29.72
        calendar-longitude 120.20)
  ;; NOTE: make sure to use `:defer' keyword for theme `use-package'.
  (setq circadian-themes '((:sunrise . leuven)
                           (:sunset . one-dark)))
  (circadian-setup))


(provide 'init-emacs-theme)

;;; init-emacs-theme.el ends here
