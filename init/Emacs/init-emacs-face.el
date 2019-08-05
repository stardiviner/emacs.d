;;; init-emacs-face.el --- init for Emacs faces.

;;; Commentary:



;;; Code:

;;; [ variable-pitch ] -- support for displaying proportional fonts.

(set-face-attribute 'variable-pitch nil
                    :family "DejaVu Sans Mono"
                    :height 105
                    :weight 'normal :slant 'normal)
(set-face-attribute 'fixed-pitch nil
                    :family "DejaVu Sans Mono"
                    :height 105
                    :weight 'normal :slant 'normal)
(set-face-attribute 'fixed-pitch-serif nil
                    :family "DejaVu Serif"
                    :height 120
                    :weight 'normal :slant 'italic)



(provide 'init-emacs-face)

;;; init-emacs-face.el ends here
