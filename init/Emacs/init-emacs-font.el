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

;; need to modify English font settings to suitable with chinese font.
;; the value is in 1/10pt, so 100 will give you 10pt, etc
;; (set-frame-font (format "%s:pixelsize=%d" "DejaVu Sans Mono" 12) t)
(set-frame-font (format "%s:pixelsize=%d" "Hack" 12) t)

;; set default font.
(set-face-attribute 'default nil
		                :family "Hack"
		                :foundry "PfEd")

;;; font "Inconsolata" is good for mixing English & Chinese.
;; https://emacs.stackexchange.com/questions/10464/japanese-cjk-font-settings-for-proper-horizontal-alignment
;; (set-frame-font "Inconsolata-12" t)

;; set Unicode characters font
(when (display-graphic-p) ; for `set-fontset-font'
  (when (member "Symbola" (font-family-list))
    (set-fontset-font t 'unicode "Symbola" nil 'prepend)))

;; set CJK font
(when (display-graphic-p)
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      ;; (font-spec :family "Noto Sans CJK SC" :size 13)
                      ;; (font-spec :family "Source Han" :size 12)
                      ;; (font-spec :family "Hack" :size 12)
                      ;; (font-spec :family "WenQuanYi Micro Hei" :size 12)
                      ;; (font-spec :family "NSimSun" :size 13)
                      (font-spec :family "BabelStone Han" :size 14)
                      )))

;;; Text Scale
(setq face-font-rescale-alist '(("WenQuanYi Micro Hei" . 1.3)))

;; (add-hook 'circadian-after-load-theme-hook #'my-circadian-font-reset)


(provide 'init-emacs-font)

;;; init-emacs-font.el ends here
