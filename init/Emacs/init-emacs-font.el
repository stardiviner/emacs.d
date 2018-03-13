;;; init-emacs-font.el --- my Emacs font settings
;;
;;; Usage:
;;
;; - [M-x set-frame-font]
;;
;;; Commentary:
;;
;; the value is in 1/10pt, so 100 will give you 10pt, etc
;;
;; - [M-x describe-font]
;; - from command: $ fc-list
;;
;; -------- Programming fonts -----
;; Hack
;; DejaVu (Sans/Serif) (Mono)
;; Droid (Sans/Serif) (Mono)
;; Monaco
;; Source Sans Pro
;; Anonymous Pro
;; Inconsolata
;; Ubuntu (Mono/Condensed)
;; Consolas
;; ------- Handwriting fonts -----
;; Comic Sans MS
;; Comic Neue
;; Gabriola
;; Old London
;; Segoe (Print/Script)
;; Death Note
;; -------- CJK fonts -----
;; Source Han (Noto Sans (Mono), Noto Serif)
;;   - Noto Sans CJK SC
;; WenQuanYi (Micro Hei/Zen Hei) (Mono)
;; HanaMin (Hanazono)
;; DFPShaoNvW5-GB
;; DFPWaWaW5-GB
;; FZMiaoWuS-GB
;; FZSuXinShiLiuKaiS-R-GB
;; iWaWa

;;; Code:

;; set font for all frames
;; (set-frame-font "DejaVu Sans Mono-10" t)
;; (set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 100)
;; (set-face-font 'default "DejaVu Sans Mono")

;; (defun my-set-font-default (font size height)
;;   "combine multiple set font together for easy change once."
;;   (interactive)
;;   (set-default-font (concat font "-" (number-to-string size)))
;;   (set-frame-font (concat font "-" (number-to-string size)) nil t)
;;   (set-face-attribute 'default nil :font font :height height)
;;   (set-face-font 'default font)
;;   )
;;
;; (my-set-font-default "DejaVu Sans Mono" 10 100)


;; --------
;; set different font for different screen size
;; --------
;;
;; (defun my-default-fonts-setup (default-height variable-pitch-height)
;;   "Set up default fonts.
;;
;; Use DEFAULT-HEIGHT for default face and VARIABLE-PITCH-HEIGHT
;; for variable-pitch face."
;;   (set-face-attribute 'default nil
;;                       :family "DejaVu Sans Mono"
;;                       :height default-height)
;;   (set-face-attribute 'variable-pitch nil
;;                       :family "Monospace"
;;                       :height variable-pitch-height
;;                       :weight 'normal))
;;
;; (when window-system
;;   (if (> (x-display-pixel-width) 1800)
;;       (my-default-fonts-setup 110 60)
;;     (my-default-fonts-setup 90 50)))

;; --------
;; set different font for different system
;; --------
;;
;; (when (eq system-type 'darwin)
;;   (set-fontset-font (frame-parameter nil 'font)
;;                     'han '("Hiragino Sans GB" . "unicode-bmp"))
;;   (set-face-attribute 'default nil :height 180)
;;   )
;;
;; (when (eq system-type 'gnu/linux)
;;   (set-fontset-font (frame-parameter nil 'font)
;;                     'han '("WenQuanYi Micro Hei Mono" . "unicode-bmp"))
;;   (set-face-attribute 'default nil :height 100)
;;   )


;; -------
;; Unicode
;; -------
;;
;; set unicode symbol fallback font to "Symbola".
;; (set-fontset-font "fontset-default" nil
;;                   (font-spec :size 20 :name "Symbola"))
;; (set-fontset-font t 'unicode
;;                   (font-spec :name "Symbola") nil 'append)
;; (set-fontset-font t 'unicode
;;                   (font-spec :name "Symbola") nil 'prepend)

;; greek characters
(when (member "DejaVu Sans Mono" (font-family-list))
  (set-fontset-font t 'greek (font-spec :name "DejaVu Sans Mono") nil))

;; override font for cyrillic characters
(when (member "DejaVu Sans Mono" (font-family-list))
  (set-fontset-font t 'cyrillic "Droid Sans Mono"))

;; -------------
;; CJK (Chinese, Japanese, Korean)
;; -------------
;;
;; other charsets except 'han : kana, symbol, cjk-misc, bopomofo,
;; - WenQuanYi Micro Hei Mono :: 文泉驿 微米黑
;; - NSimSun / SimSun :: 新宋体
;; - "Hiragino Sans GB W3"
;; - FZSuXinShiLiuKaiS-R-GB :: 方正苏新诗柳字体
;; - DFPShaoNvW5-GB :: 华康少女体 W5
;; - DFPWaWaW5-GB :: 华康娃娃体
;; - iWawa :: 华康娃娃体
;; - FZMiaoWuS-GB :: 方正喵呜体
;;
;; (set-fontset-font t 'han (font-spec :family "WenQuanYi Micro Hei Mono" :size 13))

(defun my-font-settings ()
  "Set Emacs font."
  (interactive)
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      ;; (font-spec :family "Noto Sans CJK SC" :size 13)
                      (font-spec :family "WenQuanYi Micro Hei" :size 13)
                      ;; (font-spec :family "NSimSun" :size 13)
		                  ;; (font-spec :family "AR PL UKai CN" :size 13)
		                  ;; (font-spec :family "AR PL UKai TW" :size 13)
                      ))
  
  ;; need to modify English font settings to suitable with chinese font.
  ;; the value is in 1/10pt, so 100 will give you 10pt, etc
  ;; (set-frame-font (format "%s:pixelsize=%d" "DejaVu Sans Mono" 12) t)
  (set-frame-font (format "%s:pixelsize=%d" "Hack" 12) t)
  ;; (set-frame-font (format "%s:pixelsize=%d" "Fira Mono" 12) t)

  ;; set default font.
  ;; (set-face-attribute 'default nil
	;; 	                  ;; :font "DejaVu Sans Mono"
	;; 	                  ;; :family "DejaVu Sans Mono"
	;; 	                  :family "Hack"
	;; 	                  :foundry "PfEd"
	;; 	                  ;; :height 90
  ;;                     )
  )

(my-font-settings)

(add-hook 'after-init-hook #'my-font-settings)

(defun my-circadian-font-reset (theme)
  "Reload customized faces on `circadian' `THEME' toggling."
  (interactive)
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (font-spec :family "WenQuanYi Micro Hei" :size 13)))
  (set-frame-font (format "%s:pixelsize=%d" "Hack" 12) t))
(add-hook 'circadian-after-load-theme-hook #'my-circadian-font-reset)


(provide 'init-emacs-font)

;;; init-emacs-font.el ends here
