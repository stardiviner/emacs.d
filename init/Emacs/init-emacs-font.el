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
    ;; set default font
    (set-frame-font (format "%s-%s" (eval emacs-font-name) (eval emacs-font-size)))
    ;; set unicode font
    (set-fontset-font (frame-parameter nil 'font) 'unicode (eval emacs-font-name))))

;; set CJK font
(when (display-graphic-p)
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (cl-case system-type
                        ('gnu/linux
                         ;; (font-spec :family "Source Han Sans SC" :size 25)
                         ;; (font-spec :family "Noto Sans CJK SC" :size 16)
                         ;; (font-spec :family "WenQuanYi Micro Hei" :size 12)
                         (font-spec :family "Sarasa Gothic SC" :size 28))
                        ('darwin
                         ;; macOS Chinese fonts
                         ;; (font-spec :family "Hiragino Sans CNS" :size 15)
                         ;; (font-spec :family "Apple LiGothic" :size 15)
                         ;; macOS handwriting fonts
                         (font-spec :family "Kaiti SC" :size 14))))))

;;; Text Scale
;; (cl-case system-type
;;   ('gnu/linux
;;    (setq face-font-rescale-alist '(("WenQuanYi Micro Hei" . 1.3)
;;                                    ("Source Han Sans SC" . 1.3))))
;;   ('darwin
;;    (setq face-font-rescale-alist '(("Kaiti SC" . 1.0)))))


(provide 'init-emacs-font)

;;; init-emacs-font.el ends here
