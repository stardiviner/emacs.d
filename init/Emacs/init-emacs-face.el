;;; init-emacs-face.el --- init for Emacs faces.

;;; Commentary:

;;; Code:

;;; [ variable-pitch ] -- support for displaying proportional fonts.

(use-package faces
  :custom (face-font-family-alternatives
           '(("DejaVu Sans Mono" "Hack" "Fira Sans" "Consolas" "Monaco" "Monospace")))
  :custom-face
  (variable-pitch ((t (:family "DejaVu Sans Mono"))))
  (fixed-pitch ((t (:family "Hack"))))
  (fixed-pitch-serif ((t (:family "DejaVu Serif"))))
  (default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :height 90)))))



(provide 'init-emacs-face)

;;; init-emacs-face.el ends here
