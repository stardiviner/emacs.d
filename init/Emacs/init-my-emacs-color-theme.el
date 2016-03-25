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
(setq color-theme-is-global t
      color-theme-is-cumulative t)

;; load theme way
(add-to-list 'custom-theme-load-path "~/.emacs.d/color-themes/")

(load-theme 'solarized-dark t)


;;; set color-theme for `emacsclient'

(if (or (daemonp) (server-running-p))
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (enable-theme 'solarized-dark)
                ;; (load-theme 'solarized-dark t)
                )))


;;; [ color-theme-solarized ]

;; (use-package color-theme-solarized
;;   :ensure t
;;   :init
;;   (customize-set-variable 'frame-background-mode 'light)
;;   :config
;;   (color-theme-solarized)
;;   (load-theme 'solarized t)
;;   )


;;; [ color-theme-monokai ]

;; (load-theme 'monokai t)


;;; [ leuven-theme ]

;; (use-package leuven-theme
;;   :ensure t
;;   :config
;;   ;; (load-theme 'leuven t)
;;   ;; fix leuven-theme is override by my customization.
;;   (add-hook 'after-init-hook
;;             (lambda ()
;;               (load-theme 'leuven t)))

;;   (with-eval-after-load 'org
;;     ;; override leuven-theme default colors.
;;     (set-face-attribute 'org-verbatim nil
;;                         ;; :foreground "#0066CC"
;;                         :foreground "#0671DF"
;;                         :background "#F7FDFF")
;;     (set-face-attribute 'org-code nil
;;                         ;; :foreground "#006400"
;;                         :foreground "#059205"
;;                         :background "FDFFF7"
;;                         :box '(:color "#059205" :line-width -1)
;;                         ))
;;   )


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
                    ;; "dark green", "forest green", "khaki",
                    :background "khaki"
                    ;; :background (color-darken-name (face-background 'default) 10)
                    )
;; highlight
(set-face-attribute 'highlight nil
                    :background (color-darken-name (face-background 'default) 5)
                    )

;; comment
;; family: DejaVu Serif,  Droid Serif, Gabriola, Gentium, GFS Didot, Latin Modern Mono, Segoe Print,
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic
                    )
;; built-in function.
(set-face-attribute 'font-lock-builtin-face nil
                    :slant 'italic)


;;; [ select-themes ] -- select color-theme with completing-read.

(use-package select-themes
  :ensure t
  )


(provide 'init-my-emacs-color-theme)

;;; init-my-emacs-color-theme.el ends here
