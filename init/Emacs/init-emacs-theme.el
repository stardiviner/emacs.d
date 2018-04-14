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
  ;; :ensure t
  :load-path "~/Code/Emacs/leuven-theme/"
  :ensure hl-line)

;;; [ eziam-theme ] -- A mostly monochrome theme, inspired by Tao and Leuven, with dark and light versions.

;; (use-package eziam-theme
;;   :ensure t
;;   :no-require t
;;   :load (eziam-light-theme))

;;; [ spacemacs-theme ] -- Spacemacs default color-theme.

(use-package spacemacs-theme
  :ensure t
  :no-require t
  :defer t
  ;; (load-theme 'spacemacs-dark t)
  )

;;; [ circadian ] -- theme-switching for Emacs based on daytime.

(use-package circadian
  :ensure t
  :config
  (setq calendar-location-name "Shaoxing Town"
        calendar-time-zone +480
        calendar-latitude 29.72
        calendar-longitude 120.20)
  (setq circadian-themes '((:sunrise . leuven)
                           ;; ("13:00" . spacemacs-light)
                           (:sunset . spacemacs-dark)))
  (circadian-setup))


(defun my:font-lock-extra-setup (theme)
  "Reload customized faces on `circadian' `THEME' toggling."
  (set-face-attribute 'underline nil
                      :underline (cl-case (alist-get 'background-mode (frame-parameters))
                                   ('light
                                    (color-darken-name (face-background 'default) 50))
                                   ('dark
                                    (color-lighten-name (face-background 'default) 30)))))
(add-hook 'circadian-after-load-theme-hook #'my:font-lock-extra-setup)


(provide 'init-emacs-theme)

;;; init-emacs-theme.el ends here
