;;; init-my-emacs-color-theme.el --- init for color theme
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ X Resources ]

(setq inhibit-x-resources t)

;;; [ font lock ]

(global-font-lock-mode t)

;;; [ Custom Themes ]

;;; [ Color Themes ]

(use-package color-theme
  :ensure t
  :init
  (autoload 'color-darken-name "color.el")
  (autoload 'color-lighten-name "color.el")
  :config
  ;; (setq color-theme-is-global t
  ;;     color-theme-is-cumulative t)

  (color-theme-initialize)
  
  ;; (color-theme-robin-hood) ; a color-theme
  ;; (load-theme 'solarized-dark t)
  )

;; (use-package color-theme-buffer-local
;;   :ensure t
;;   :config
;;   (add-hook 'org-mode-hook
;;             (lambda ()
;;               (color-theme-buffer-local 'color-theme-standard (current-buffer))))
;;   )

;; (add-to-list 'custom-theme-load-path "~/.emacs.d/color-themes/")

;;; [ leuven-theme ] -- Awesome Emacs color theme on white background.

(use-package leuven-theme
  :ensure t
  :defer t
  :config
  (setq leuven-scale-outline-headlines t)
  ;; (load-theme 'leuven t)
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

;; (set-face-attribute 'underline nil
;;                     :underline (cl-case (alist-get 'background-mode (frame-parameters))
;;                                  ('light
;;                                   (color-darken-name (face-background 'default) 50))
;;                                  ('dark
;;                                   (color-lighten-name (face-background 'default) 10))
;;                                  )
;;                     )

;; (set-face-attribute 'region nil
;;                     :inherit nil :inverse-video nil
;;                     :background (cl-case (alist-get 'background-mode (frame-parameters))
;;                                   ('light
;;                                    (color-darken-name (face-background 'default) 5))
;;                                   ('dark
;;                                    (color-lighten-name (face-background 'default) 8)))
;;                     )

;; comment
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic)
;; built-in function.
(set-face-attribute 'font-lock-builtin-face nil
                    :slant 'italic)
(set-face-attribute 'font-lock-function-name-face nil
                    :background (color-lighten-name (face-background 'default) 2))
(set-face-attribute 'font-lock-type-face nil
                    :background (color-lighten-name (face-background 'default) 3))

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


(provide 'init-my-emacs-color-theme)

;;; init-my-emacs-color-theme.el ends here
