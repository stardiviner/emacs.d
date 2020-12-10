;;; init-emacs-face.el --- init for Emacs faces.

;;; Commentary:

;; the value is in 1/10pt, so 100 will give you 10pt, etc
;;
;; - [C-u C-x =]
;; - [M-x describe-font]
;; - [M-x describe-fontset]
;; - from command: $ fc-list
;;
;; Emacs set font functions:
;;
;; - `set-face-attribute'
;; - `set-frame-font'
;;
;; - "DejaVu Sans Mono"
;; - "Hack"
;; - "Fira Sans"
;; - "Sarasa Mono SC"
;; - "Sarasa Nerd"

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
