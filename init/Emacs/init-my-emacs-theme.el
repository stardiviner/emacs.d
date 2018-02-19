;;; init-my-emacs-theme.el --- Init for Themes
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ X Resources ]

(setq inhibit-x-resources t)

;;; [ font lock ]

(global-font-lock-mode t)

(require 'color) ; for `color-lighten-name' and `color-darken-name'

;;; [ leuven-theme ] -- Awesome Emacs color theme on white background.

(use-package leuven-theme
  :ensure t
  :defer t
  :config
  (setq leuven-scale-outline-headlines t)
  ;; (load-theme 'leuven t)

  ;; remove underline for `hl-line'.
  (with-eval-after-load 'hl-line
    (defun my:leuven-theme-disable-underline (theme)
      "Reload customized faces on `circadian' `THEME' toggling."
      (if (custom-theme-enabled-p 'leuven)
          (set-face-attribute 'hl-line nil :underline nil)))
    (add-hook 'circadian-after-load-theme-hook #'my:leuven-theme-disable-underline))
  )

;;; [ spacemacs-theme ] -- Spacemacs default color-theme.

(use-package spacemacs-theme
  :ensure t
  :no-require t
  :defer t
  ;; (load-theme 'spacemacs-dark t)
  )

;;; [ kaolin-themes ] -- A set of eye pleasing themes.

;; (use-package kaolin-themes
;;   :ensure t
;;   :config
;;   (load-theme 'kaolin-dark t))

;;; [ circadian ] -- theme-switching for Emacs based on daytime.

(use-package circadian
  :ensure t
  :config
  (setq calendar-location-name "Shaoxing Town"
        calendar-time-zone +480
        calendar-latitude 29.72
        calendar-longitude 120.20)
  (setq circadian-themes '((:sunrise . leuven)
                           (:sunset . spacemacs-dark)))
  (circadian-setup))


(defun my:font-lock-extra-setup (theme)
  "Reload customized faces on `circadian' `THEME' toggling."
  (set-face-attribute 'underline nil
                      :underline (cl-case (alist-get 'background-mode (frame-parameters))
                                   ('light
                                    (color-darken-name (face-background 'default) 50))
                                   ('dark
                                    (color-lighten-name (face-background 'default) 10))
                                   )
                      )
  )
(add-hook 'circadian-after-load-theme-hook #'my:font-lock-extra-setup)


(provide 'init-my-emacs-theme)

;;; init-my-emacs-theme.el ends here
