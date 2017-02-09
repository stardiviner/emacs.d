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

;; (load-theme 'solarized-dark t)

(use-package leuven-theme
  :ensure t
  :config
  (setq leuven-scale-outline-headlines t)
  (load-theme 'leuven t)
  (set-face-attribute 'highlight nil
                      :foreground (color-darken-name (face-foreground 'default) 50)
                      :background (color-darken-name (face-background 'default) 5)
                      )
  )

;;; [ madhat2r-theme ] -- Dark theme for (spac[e]macs) that supports GUI and terminal.

;; (use-package madhat2r-theme
;;   :ensure t
;;   :config
;;   (setq madhat2r-theme-org-height t
;;         madhat2r-theme-org-agenda-height nil)
;;   (load-theme 'madhat2r t)
;;   )

;;; [ doom-themes ] -- Emacs themes inspired by Atom One

;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   (setq doom-enable-bold t
;;         doom-enable-italic t)
;;   (load-theme 'doom-one t)
;;   (add-hook 'find-file-hook 'doom-buffer-mode) ; brighter source buffers.
;;   (add-hook 'minibuffer-setup-hook 'doom-buffer-mode) ; brighter minibuffer when active
;;   )


;;; custom faces
(set-face-attribute 'italic nil
                    :slant 'italic
                    :foreground (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 50))
                                  ('dark
                                   (color-lighten-name (face-background 'default) 10)))
                    )
(set-face-attribute 'bold nil
                    :weight 'bold
                    :foreground (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 50))
                                  ('dark
                                   (color-lighten-name (face-background 'default) 10)))
                    )
(set-face-attribute 'bold-italic nil
                    :weight 'bold :slant 'italic
                    :foreground (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 50))
                                  ('dark
                                   (color-lighten-name (face-background 'default) 10)))
                    )
(set-face-attribute 'underline nil
                    :underline (cl-case (alist-get 'background-mode (frame-parameters))
                                 ('light
                                  (color-darken-name (face-background 'default) 50))
                                 ('dark
                                  (color-lighten-name (face-background 'default) 10))
                                 ))
(set-face-attribute 'region nil
                    :inherit nil :inverse-video nil
                    :background (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light "khaki")
                                  ('dark "forest green"))
                    )

;; comment
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic
                    )
;; built-in function.
(set-face-attribute 'font-lock-builtin-face nil
                    :slant 'italic)

(set-face-attribute 'font-lock-function-name-face nil
                    :background (color-lighten-name (face-background 'default) 2)
                    )
(set-face-attribute 'font-lock-type-face nil
                    :background (color-lighten-name (face-background 'default) 3)
                    )


;;; [ select-themes ] -- select color-theme with completing-read.

;; (use-package select-themes
;;   :ensure t
;;   :defer t)

;;; set color-theme for `emacsclient'
;; NOTE: this will override my custom faces again. so disable it.
;;
;; (if (daemonp)
;;     (add-hook 'after-make-frame-functions
;;               '(lambda (frame)
;;                  (with-selected-frame frame
;;                    (progn
;;                      (toggle-frame-maximized)
;;                      (load-theme 'doom-one t)
;;                      )))))


(provide 'init-my-emacs-color-theme)

;;; init-my-emacs-color-theme.el ends here
