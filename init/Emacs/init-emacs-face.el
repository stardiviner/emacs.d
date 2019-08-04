;;; init-emacs-face.el --- init for Emacs faces.

;;; Commentary:



;;; Code:

;;; [ variable-pitch ] -- support for displaying proportional fonts.

(cl-case system-type
  ('gnu/linux
   (set-face-attribute 'variable-pitch nil
                       :family "Source Sans Pro"
                       :height 105
                       :weight 'normal :slant 'normal)
   (set-face-attribute 'fixed-pitch nil
                       :family "Hack"
                       :height 100
                       :weight 'normal :slant 'normal)
   (set-face-attribute 'fixed-pitch-serif nil
                       :family "Hack"
                       :height 120
                       :weight 'normal :slant 'italic))
  ('darwin
   (set-face-attribute 'variable-pitch nil
                       :family "DejaVu Sans Mono"
                       :height 120
                       :weight 'normal :slant 'normal)
   (set-face-attribute 'fixed-pitch nil
                       :family "Hack"
                       :height 120
                       :weight 'normal :slant 'normal)
   (set-face-attribute 'fixed-pitch-serif nil
                       :family "Hack"
                       :height 120
                       :weight 'normal :slant 'italic)))




(provide 'init-emacs-face)

;;; init-emacs-face.el ends here
