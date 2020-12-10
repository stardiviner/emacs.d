;;; init-emacs-theme.el --- Init for Themes
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ X Resources ]

(setq inhibit-x-resources t)

;;; [ font lock ]

(global-font-lock-mode t)

(autoload 'color-lighten-name "color.el")
(autoload 'color-darken-name "color.el")

;;; [ custom theme ]

(defcustom load-theme-before-hook nil
  "Functions to run before load theme."
  :type 'hook)

(defcustom load-theme-after-hook nil
  "Functions to run after load theme."
  :type 'hook)

(defun load-theme-hook-wrapper (origin-func theme &rest args)
  "A wrapper of hooks around `load-theme'."
  (mapc #'disable-theme custom-enabled-themes)
  (run-hook-with-args 'load-theme-before-hook theme)
  (apply origin-func theme args)
  (run-hook-with-args 'load-theme-after-hook theme))

(advice-add 'load-theme :around #'load-theme-hook-wrapper)

;;; [ leuven-theme ]

(use-package leuven-theme
  ;; :quelpa (leuven-theme :fetcher github :repo "stardiviner/leuven-theme")
  :load-path "~/Code/Emacs/leuven-theme/"
  :demand t
  :config (load-theme 'leuven t))

;;; [ one-themes ] -- One color scheme.

(use-package one-themes
  ;; :quelpa (one-themes :fetcher github :repo "stardiviner/emacs-one-themes")
  :load-path "~/Code/Emacs/one-themes"
  :demand t
  :config (load-theme 'one-dark t))

;;; [ doom-themes ] -- an opinionated pack of modern color-themes.

;; (use-package doom-themes
;;   :ensure t
;;   :config (load-theme 'doom-palenight t))

;;; [ dracula-theme ] -- Dark theme for Emacs and 137+ apps.

;; (use-package dracula-theme
;;   :ensure t
;;   :config (load-theme 'dracula t))

;;; [ circadian ] -- Theme-switching based on daytime.

(use-package circadian
  :ensure t
  :custom ((calendar-location-name "Shaoxing Town")
           (calendar-time-zone +480)
           (calendar-latitude 29.72)
           (calendar-longitude 120.20)
           (circadian-themes '((:sunrise . leuven)
                               (:sunset . one-dark))))
  :hook (after-init . circadian-setup))

;;; Use large font for function name only in programming modes.
;;
;; (defun my-enlarge-function-name ()
;;   "Use large font for function name only in programming modes."
;;   (face-remap-add-relative 'font-lock-function-name-face
;;                            :overline t :height 120))
;;
;; (add-hook 'prog-mode-hook #'my-enlarge-function-name)


(provide 'init-emacs-theme)

;;; init-emacs-theme.el ends here
