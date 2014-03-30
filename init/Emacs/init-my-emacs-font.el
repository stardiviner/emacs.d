;;; init-my-emacs-font.el --- my Emacs font settings
;;
;;; Commentary:
;; from command: $ fc-list
;; Comic Sans MS
;; Gabriola
;; Old London
;; Segoe (Print/Script)
;; Evanescent
;; Century Schoolbook L
;; Death Note
;; --------
;; Anonymous Pro
;; DejaVu (Sans/Serif) (Mono)
;; Droid (Sans/Serif) (Mono)
;; Monaco
;; Source Sans Pro
;; Inconsolata
;; Ubuntu (Mono/Condensed)
;; --------
;; WenQuanYi (Micro Hei/Zen Hei) (Mono)
;; DFPShaoNvW5-GB
;; DFPWaWaW5-GB
;; FZMiaoWuS-GB
;; FZSuXinShiLiuKaiS-R-GB
;; iWaWa

;;; Code:
;; set font for all frames
(set-frame-font "DejaVu Sans Mono-10" t)

;; Averia-12
;; Averia Serif-12
;; Linux Libertine-13

;; -------------
;; Chinese Font
;; -------------
;;
;; other charsets except 'han : kana, symbol, cjk-misc, bopomofo,
;; - WenQuanYi Micro Hei Mono :: 文泉驿 微米黑
;; - DFPShaoNvW5-GB :: 华康少女体 W5
;; - DFPWaWaW5-GB :: 华康娃娃体
;; - iWawa :: 华康娃娃体
;; - FZMiaoWuS-GB :: 方正喵呜体
;; - FZSuXinShiLiuKaiS-R-GB :: 方正苏新诗柳字体
;; 1.
;; (set-fontset-font t 'han (font-spec :family "WenQuanYi Micro Hei Mono" :size 13))
;; 2.
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset
                    (font-spec :family "WenQuanYi Micro Hei Mono" :size 13) ; 文泉驿 微米黑
                    )
  )
;;; need to modify English font settings to suitable with chinese font.
;;; the value is in 1/10pt, so 100 will give you 10pt, etc
(set-face-attribute 'default nil :font "DejaVu Sans Mono-10" :height 100)


;; --------
;; set different font for different system
;; --------

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

;;; others
;; choose your own fonts, in a system dependant way
;; (if (string-match "apple-darwin" system-configuration)
;;   (set-face-font 'default "Monaco-13")
;;   (set-face-font 'default "DejaVu Sans Mono-10"))
;; set Chinese font
;; (setq chinese-font "FZSuXinShiLiuKaiS-R-GB-24")
;; (setq chinese-font "DFPShaoNvW5-GB")
;; (setq cjk-font-size 18)
;; set English font
;; (setq ansii-font "DejaVu Sans Mon-10")
;; (setq ansi-font-size 16)

;;; the value is in 1/10pt, so 100 will give you 10pt, etc
;; (set-face-attribute 'default nil
;;                     :family "DejaVu Sans Mono"
;;                     :height 100)


(provide 'init-my-emacs-font)

;;; init-my-emacs-font.el ends here
