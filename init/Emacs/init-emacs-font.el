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

(let ((emacs-font-size 9)
      emacs-font-name)
  (cond
   ((or (eq system-type 'darwin) (featurep 'cocoa))
    (setq emacs-font-name "Monaco"))
   ((eq system-type 'gnu/linux)
    ;; "DejaVu Sans Mono", "Fira Code", "Hack", "Source Code Pro", "Noto Sans Mono", "Sarasa Gothic SC", "Sarasa Mono SC",
    ;; "Comic Neue", "Comic Sans MS", "FZSuXinShiLiuKaiS-R-GB",
    ;; "ETBembo", "ETBookOT", "Gabriola"
    (setq emacs-font-name "DejaVu Sans Mono")))
  (when (display-grayscale-p)
    (set-frame-font (format "%s-%s" (eval emacs-font-name) (eval emacs-font-size)))
    (set-fontset-font (frame-parameter nil 'font) 'unicode (eval emacs-font-name))))


;; set Unicode characters font
(when (display-graphic-p) ; for `set-fontset-font'
  (when (member "Symbola" (font-family-list))
    (set-fontset-font t 'unicode "Symbola" nil 'prepend)))



(provide 'init-emacs-font)

;;; init-emacs-font.el ends here
