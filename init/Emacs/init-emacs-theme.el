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

;;; [ modus-themes ] -- Accessible themes for GNU Emacs, conforming with the highest accessibility standard for colour contrast between background and foreground values (WCAG AAA standard).

;; (use-package modus-vivendi-theme
;;   :ensure t
;;   :custom ((modus-vivendi-theme-slanted-constructs t)
;;            (modus-vivendi-theme-bold-constructs t)
;;            (modus-vivendi-theme-variable-pitch-headings t)
;;            (modus-vivendi-theme-scale-headings t)
;;            (modus-vivendi-theme-fringes ' subtle)
;;            (modus-vivendi-theme-distinct-org-blocks t)
;;            (modus-vivendi-theme-org-blocks 'rainbow)
;;            (modus-vivendi-theme-3d-modeline t)
;;            (modus-vivendi-theme-mode-line '3d)
;;            (modus-vivendi-theme-diffs 'desaturated)
;;            (modus-vivendi-theme-completions 'moderate)
;;            (modus-vivendi-theme-prompts 'intense)
;;            (modus-vivendi-theme-intense-hl-line t)
;;            (modus-vivendi-theme-intense-paren-match t)
;;            (modus-vivendi-theme-faint-syntax t))
;;   :config (load-theme 'modus-vivendi t))

;; (use-package modus-operandi-theme
;;   :ensure t
;;   :custom ((modus-operandi-theme-slanted-constructs t)
;;            (modus-operandi-theme-bold-constructs t)
;;            (modus-operandi-theme-variable-pitch-headings t)
;;            (modus-operandi-theme-scale-headings t)
;;            (modus-operandi-theme-fringes ' subtle)
;;            (modus-operandi-theme-distinct-org-blocks t)
;;            (modus-operandi-theme-org-blocks 'rainbow)
;;            (modus-operandi-theme-3d-modeline t)
;;            (modus-operandi-theme-mode-line '3d)
;;            (modus-operandi-theme-diffs 'desaturated)
;;            (modus-operandi-theme-completions 'moderate)
;;            (modus-operandi-theme-prompts 'intense)
;;            (modus-operandi-theme-intense-hl-line t)
;;            (modus-operandi-theme-intense-paren-match t)
;;            (modus-operandi-theme-faint-syntax t))
;;   :config (load-theme 'modus-operandi t))

;;; [ leuven-theme ]

(use-package leuven-theme
  ;; :quelpa (leuven-theme :fetcher github :repo "stardiviner/leuven-theme")
  :load-path "~/Code/Emacs/leuven-theme/"
  :config (load-theme 'leuven t))

;;; [ one-themes ] -- One color scheme.

(use-package one-themes
  ;; :quelpa (one-themes :fetcher github :repo "stardiviner/emacs-one-themes")
  :load-path "~/Code/Emacs/one-themes"
  :config (load-theme 'one-dark t))

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
