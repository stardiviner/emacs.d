;;; init-emacs-font.el --- Emacs default font settings.
;;
;;; Commentary:
;;
;; the value is in 1/10pt, so 100 will give you 10pt, etc
;;
;; - [C-u C-x =]
;; - [M-x describe-font]
;; - [M-x describe-fontset]
;; - from command: $ fc-list

;;; Emacs set font functions:
;;;
;;; - `set-face-attribute'
;;; - `set-frame-font'

;;; Code:

(set-face-attribute 'default nil
                    :family "DejaVu Sans Mono"
                    :font "DejaVu Sans Mono"
                    :foundry "PfEd"
                    :height 90)


(provide 'init-emacs-font)

;;; init-emacs-font.el ends here
