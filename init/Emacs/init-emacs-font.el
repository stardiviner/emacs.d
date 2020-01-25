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
(cl-case system-type
  ('gnu/linux
   (set-frame-font (format "%s:pixelsize=%d" "Noto Sans Mono" 12) t) ; "DejaVu Sans Mono"
   (set-face-attribute 'default nil
                       :family "DejaVu Sans Mono"
                       :foundry "PfEd"))
  ('darwin
   (set-frame-font (format "%s:pixelsize=%d" "Fira Code" 12) t)
   (set-face-attribute 'default nil
                       :family "Fira Code"
                       :foundry "PfEd")))


;; set Unicode characters font
(when (display-graphic-p) ; for `set-fontset-font'
  (when (member "Symbola" (font-family-list))
    (set-fontset-font t 'unicode "Symbola" nil 'prepend)))

;; set CJK font
(when (display-graphic-p)
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (cl-case system-type
                        ('gnu/linux
                         ;; (font-spec :family "Source Han Sans" :size 16)
                         ;; (font-spec :family "Sarasa Gothic SC" :size 16)
                         (font-spec :family "Noto Sans CJK SC" :size 16)
                         ;; (font-spec :family "Hack" :size 12)
                         ;; (font-spec :family "WenQuanYi Micro Hei" :size 12)
                         ;; (font-spec :family "NSimSun" :size 14)
                         ;; (font-spec :family "BabelStone Han" :size 14)
                         ;; (font-spec :family "FZQingKeBenYueSongS-R-GB" :size 15) ; 方正清刻本悦宋简体
                         ;; (font-spec :family "WenYue GuDianMingChaoTi-R-GB" :size 15)
                         ;; (font-spec :family "WenYue HouXianDaiTi-R-GB" :size 15)
                         ;; (font-spec :family "Roboto" :size 15)
                         ;; (font-spec :family "WenyueType GutiFangsong-R-GB" :size 15)
                         )
                        ('darwin
                         ;; macOS Chinese fonts
                         ;; (font-spec :family "Songti SC" :size 15)
                         ;; (font-spec :family "Heiti SC" :size 15)
                         ;; (font-spec :family "Hiragino Sans CNS" :size 15)
                         ;; (font-spec :family "Lantinghei SC" :size 15)
                         ;; (font-spec :family "LiHei Pro" :size 15)
                         ;; (font-spec :family "LiSong Pro" :size 15)
                         ;; (font-spec :family "PingFang SC" :size 15)
                         ;; (font-spec :family "Yuanti SC" :size 15)
                         ;; (font-spec :family "Apple LiGothic" :size 15)

                         ;; macOS handwriting fonts
                         ;; (font-spec :family "BiauKai" :size 13)
                         (font-spec :family "Kaiti SC" :size 14)
                         ;; (font-spec :family "Hannotate SC" :size 14)
                         ;; (font-spec :family "Libian SC" :size 14)
                         ;; (font-spec :family "Weibei SC" :size 14)
                         ;; (font-spec :family "LingWai SC" :size 15)
                         ;; (font-spec :family "HanziPen SC" :size 14)
                         ;; (font-spec :family "Yuppy SC" :size 14)
                         ;; (font-spec :family "Wawati SC" :size 14)
                         ;; (font-spec :family "Xingkai SC" :size 14)
                         )))))

;;; Text Scale
(cl-case system-type
  ('gnu/linux
   (setq face-font-rescale-alist '(("WenQuanYi Micro Hei" . 1.3))))
  ('darwin
   (setq face-font-rescale-alist '(("Kaiti SC" . 1.0)))))


(provide 'init-emacs-font)

;;; init-emacs-font.el ends here
