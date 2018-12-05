;;; init-emacs-theme.el --- Init for Themes
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ X Resources ]

(setq inhibit-x-resources t)

;;; [ font lock ]

(global-font-lock-mode t)

(require 'color) ; for `color-lighten-name' and `color-darken-name'

(use-package delight
  :ensure t
  :delight buffer-face-mode)

;;; [ leuven-theme ]

;; (use-package leuven-theme
;;   :load-path "~/Code/Emacs/leuven-theme/"
;;   :no-require t
;;   :init (require 'leuven-theme)
;;   :config (load-theme 'leuven t))

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

;; (use-package one-themes
;;   :ensure t
;;   :no-require t
;;   :config (load-theme 'one-dark t))

;;; [ doom-themes ] -- an opinionated pack of modern color-themes.

(use-package doom-themes
  :ensure t
  :no-require t
  :config (load-theme 'doom-one t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(set-face-attribute 'underline nil
                    :underline (cl-case (alist-get 'background-mode (frame-parameters))
                                 ('light
                                  (color-darken-name (face-background 'default) 50))
                                 ('dark
                                  (color-lighten-name (face-background 'default) 30))))
(set-face-attribute 'italic nil
                    :slant 'italic
                    :foreground (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light "black")
                                  ('dark "white"))
                    :background (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 10))
                                  ('dark
                                   (color-lighten-name (face-background 'default) 5)))
                    )
(set-face-attribute 'bold nil
                    :weight 'bold
                    :foreground (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light "black")
                                  ('dark "white"))
                    )
(set-face-attribute 'bold-italic nil
                    :weight 'bold :slant 'italic
                    :foreground (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light "black")
                                  ('dark "white"))
                    )
(set-face-attribute 'underline nil
                    :foreground (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light "black")
                                  ('dark "white"))
                    :background (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 10))
                                  ('dark
                                   (color-lighten-name (face-background 'default) 5)))
                    )


(provide 'init-emacs-theme)

;;; init-emacs-theme.el ends here
