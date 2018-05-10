;;; init-emacs-face.el --- init for Emacs faces.

;;; Commentary:



;;; Code:

(defun my:italic-underline-face-background (theme)
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
  )
(add-hook 'circadian-after-load-theme-hook #'my:italic-underline-face-background)


;;; [ variable-pitch ] -- support for displaying proportional fonts.

(set-face-attribute 'variable-pitch nil
                    :family "Source Sans Pro"
                    ;; :height 1.0
                    :weight 'light :slant 'normal)
(set-face-attribute 'fixed-pitch nil
                    ;; :family "DejaVu Sans Mono"
                    ;; :family "Source Code Pro"
                    :family "Hack"
                    ;; :family "Monaco"
                    :height 1.0
                    :weight 'normal :slant 'normal)



(provide 'init-emacs-face)

;;; init-emacs-face.el ends here
