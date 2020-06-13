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

;; (let ((emacs-font-size 9)
;;       emacs-font-name)
;;   (cond
;;    ((or (eq system-type 'darwin) (featurep 'cocoa))
;;     (setq emacs-font-name "Monaco"))
;;    ((eq system-type 'gnu/linux)
;;     ;; "DejaVu Sans Mono", "Fira Code", "Hack", "Source Code Pro", "Noto Sans Mono", "Sarasa Gothic SC", "Sarasa Mono SC",
;;     ;; "Comic Neue", "Comic Sans MS", "FZSuXinShiLiuKaiS-R-GB",
;;     ;; "ETBembo", "ETBookOT", "Gabriola"
;;     (setq emacs-font-name "DejaVu Sans Mono")))
;;   (when (display-grayscale-p)
;;     ;; set default font
;;     (set-frame-font (format "%s-%s" (eval emacs-font-name) (eval emacs-font-size)))
;;     (set-face-attribute 'default nil
;;                         :family emacs-font-name
;;                         :font emacs-font-name
;;                         :foundry "PfEd")
;;     ;; set unicode font
;;     (set-fontset-font (frame-parameter nil 'font) 'unicode (eval emacs-font-name))))

;;; NOTE `doom-modeline' will calculate width and height once when loaded. Avoid
;;; doom-modeline right side space. You need to re-toggle doom-modeline.
;;; `doom-modeline-refresh-font-width-cache'
(set-face-attribute 'default nil
                    :family "DejaVu Sans Mono"
                    :font "DejaVu Sans Mono"
                    :foundry "PfEd"
                    :height 90)


(provide 'init-emacs-font)

;;; init-emacs-font.el ends here
