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

;;; Code:

(cl-case system-type
  ('gnu/linux
   (set-frame-font (format "%s:pixelsize=%d" "Sarasa Mono SC" 12) t)
   (set-face-attribute 'default nil
                       :family "Sarasa Mono SC"
                       :font "Sarasa Mono SC"
                       :height 100))
  ('darwin
   (set-frame-font (format "%s:pixelsize=%d" "Fira Code" 12) t)))


;; set Unicode characters font
(when (display-graphic-p) ; for `set-fontset-font'
  (when (member "Symbola" (font-family-list))
    (set-fontset-font t 'unicode "Symbola" nil 'prepend)))



(provide 'init-emacs-font)

;;; init-emacs-font.el ends here
