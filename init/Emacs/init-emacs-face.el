;;; init-emacs-face.el --- init for Emacs faces.

;;; Commentary:



;;; Code:

;;; [ variable-pitch ] -- support for displaying proportional fonts.

(set-face-attribute 'variable-pitch nil
                    :family "Source Sans Pro"
                    :height 105
                    :weight 'light)
(set-face-attribute 'fixed-pitch nil
                    :family "DejaVu Sans Mono"
                    ;; :family "Comic Sans MS"
                    :height 1.0 :weight 'normal :slant 'normal)



(provide 'init-emacs-face)

;;; init-emacs-face.el ends here
