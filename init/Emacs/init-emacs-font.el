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
(set-frame-font (format "%s:pixelsize=%d" "Fira Code" 12) t)

;;; Styled fonts
;; (set-frame-font (format "%s:pixelsize=%d" "STALKER1" 16) t)

;;; font "Inconsolata" is good for mixing English & Chinese.
;; https://emacs.stackexchange.com/questions/10464/japanese-cjk-font-settings-for-proper-horizontal-alignment
;; (set-frame-font "Inconsolata-12" t)

;; set default font.
(set-face-attribute 'default nil
		                :family "Fira Code"
		                :foundry "PfEd")

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
                      (font-spec :family "WenQuanYi Micro Hei" :size 12)
                      ;; (font-spec :family "NSimSun" :size 14)
                      ;; (font-spec :family "BabelStone Han" :size 14)
                      ;; 方正清刻本悦宋简体
                      ;; (font-spec :family "FZQingKeBenYueSongS-R-GB" :size 15)
                      ;; (font-spec :family "WenYue GuDianMingChaoTi-R-GB" :size 15)
                      ;; (font-spec :family "WenYue HouXianDaiTi-R-GB" :size 15)
                      ;; (font-spec :family "Roboto" :size 15)
                      ;; (font-spec :family "WenyueType GutiFangsong-R-GB" :size 15)
                      )))

;;; Text Scale
(setq face-font-rescale-alist '(("WenQuanYi Micro Hei" . 1.3)))


(provide 'init-emacs-font)

;;; init-emacs-font.el ends here
