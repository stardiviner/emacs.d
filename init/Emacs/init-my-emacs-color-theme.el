;;; init-my-emacs-color-theme.el --- init for color theme
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ X Resources ]

(setq inhibit-x-resources t)

;;; [ Color Theme ]

;; Usage:
;; - [M-x customize-face] -- to custom current point face color/style.
;; - [M-x list-colors-display] / [helm-colors]/[C-x c c] -- list out all basic colors.
;; - [C-u M-x list-faces-display RET org] -- overview of all the faces in org-mode.
;; - [M-x customize-group org-font-lock] -- custom org-faces and other aspects of org-apperance.
;; - [C-u C-x =] -- verbose information about the properties of the text under the point.
;; - [M-x load-theme RET (theme)] -- load a color-theme.

(require 'color) ; for function `color-darken-name'

;;; initialize color-theme
(use-package color-theme
  :ensure t
  :config
  (with-eval-after-load "color-theme"
    (color-theme-initialize))
  )

(setq color-theme-is-global t
      color-theme-is-cumulative t)

;; load theme way
(add-to-list 'custom-theme-load-path "~/.emacs.d/color-themes/")

;; (load-theme 'solarized-dark t)

;;; [ leuven-theme ] -- Awesome Emacs color theme on white background.

;; (use-package leuven-theme
;;   :ensure t
;;   :config
;;   (setq leuven-scale-outline-headlines t)
;;   (load-theme 'leuven t)
;;   )

;;; [ nord-theme ] -- An arctic, north-bluish clean and elegant Emacs theme.

(use-package nord-theme
  :ensure t
  :config
  (load-theme 'nord t)
  )

;;; [ doom-themes ] -- Emacs themes inspired by Atom One

;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   (setq doom-enable-bold t
;;         doom-enable-italic t)
;;   (load-theme 'doom-one t)
;;   )

;;; [ gruvbox-theme ] -- Gruvbox is a retro groove color scheme for Emacs.

;; (use-package gruvbox-theme
;;   :ensure t
;;   :config
;;   (load-theme 'gruvbox t))


;;; custom faces
(set-face-attribute 'italic nil
                    :slant 'italic
                    :foreground (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light "black")
                                  ('dark "white"))
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
;;                                   ('light "khaki")
;;                                   ('dark "forest green"))
;;                     )

;; comment
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic)
;; built-in function.
(set-face-attribute 'font-lock-builtin-face nil
                    :slant 'italic)

(set-face-attribute 'font-lock-function-name-face nil
                    :background (color-lighten-name (face-background 'default) 2)
                    )
(set-face-attribute 'font-lock-type-face nil
                    :background (color-lighten-name (face-background 'default) 3)
                    )

;;; apply color-theme on new frame to fix issue.
;; (defun my-new-frame-apply-things (arg)
;;   (load-theme 'leuven t))
;;
;; (add-hook 'after-make-frame-functions #'my-new-frame-apply-things)

;;; [ select-themes ] -- select color-theme with completing-read.

;; (use-package select-themes
;;   :ensure t
;;   :defer t)

;;; [ circadian ] -- theme-switching for Emacs based on daytime.

;; (use-package circadian
;;   :ensure t
;;   :config
;;   (setq circadian-themes '(("9:00" . leuven) ; dichromacy, leuven
;;                            ("15:30" . nord)))
;;
;;   (circadian-setup)
;;   )


(provide 'init-my-emacs-color-theme)

;;; init-my-emacs-color-theme.el ends here
