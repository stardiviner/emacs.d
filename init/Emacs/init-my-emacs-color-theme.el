;;; init-my-emacs-color-theme.el --- init for color theme
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

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
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)))
(setq color-theme-is-global t)

;; load theme way
(add-to-list 'custom-theme-load-path "~/.emacs.d/color-themes/")
;; (load-theme 'color-theme-midnight)


;;; color-theme-solarized

(require 'color-theme-solarized)

(color-theme-solarized)

;; (load-theme 'solarized t)


;;; color-theme-monokai

;; (load-theme 'monokai t)


;;; color-theme-almost-monokai

;; (color-theme-almost-monokai)


;;; custom faces

(set-face-attribute 'font-lock-function-name-face nil
                    :background (color-lighten-name (face-background 'default) 2)
                    )
(set-face-attribute 'font-lock-type-face nil
                    :background (color-lighten-name (face-background 'default) 3)
                    :foreground "green")

;; italic & bold
(set-face-attribute 'italic nil
                    :slant 'italic
                    :foreground "white"
                    :underline nil)
(set-face-attribute 'bold nil
                    :weight 'bold
                    :foreground "white")
(set-face-attribute 'underline nil
                    :underline "white")
;; region
(set-face-attribute 'region nil
                    :inverse-video nil
                    :foreground nil
                    :background "dark green"
                    ;; :background (color-darken-name (face-background 'default) 10)
                    )
;; highlight
(set-face-attribute 'highlight nil
                    :background (color-darken-name (face-background 'default) 5)
                    )
;; search
(set-face-attribute 'isearch nil
                    :inherit nil
                    :inverse-video nil
                    :background (color-darken-name (face-background 'default) 5)
                    :foreground "pink"
                    :box '(:color "green" :line-width 1)
                    :slant 'italic
                    :weight 'normal)
(set-face-attribute 'isearch-fail nil
                    :inherit nil
                    :inverse-video nil
                    :background (color-darken-name (face-background 'default) 5)
                    :foreground "dark red"
                    :weight 'bold
                    :slant 'italic)
;; match
(set-face-attribute 'lazy-highlight nil
                    :inherit nil
                    :inverse-video nil
                    :background (color-darken-name (face-background 'default) 5)
                    :foreground "cyan"
                    :weight 'bold
                    )
(set-face-attribute 'match nil
                    :inherit nil
                    :inverse-video nil
                    :background (color-darken-name (face-background 'default) 3)
                    :foreground "red"
                    )
;; replace
(set-face-attribute 'query-replace nil
                    :inherit nil
                    :inverse-video nil
                    :background (color-darken-name (face-background 'default) 5)
                    :foreground "orange"
                    :weight 'bold
                    :box '(:color "black" :line-width 1 :style nil))

;; comment
;; family: DejaVu Serif,  Droid Serif, Gabriola, Gentium, GFS Didot, Latin Modern Mono, Segoe Print,
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic
                    )
;; built-in function.
(set-face-attribute 'font-lock-builtin-face nil
                    :slant 'italic)

;; redifine diff
(set-face-attribute 'diff-refine-added nil
                    :foreground nil :background "dark green")
(set-face-attribute 'diff-refine-removed nil
                    :foreground nil :background "dark red")
(set-face-attribute 'diff-refine-change nil
                    :foreground nil :background "white")


(provide 'init-my-emacs-color-theme)

;;; init-my-emacs-color-theme.el ends here
