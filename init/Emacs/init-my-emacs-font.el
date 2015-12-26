;;; init-my-emacs-font.el --- my Emacs font settings
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
;; Comic Sans MS
;; Comic Neue
;; Gabriola
;; Old London
;; Segoe (Print/Script)
;; Death Note
;; --------
;; Hack
;; DejaVu (Sans/Serif) (Mono)
;; Droid (Sans/Serif) (Mono)
;; Monaco
;; Source Sans Pro
;; Anonymous Pro
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
;; (set-frame-font "DejaVu Sans Mono-10" t)
;; (set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 100)
;; (set-face-font 'default "DejaVu Sans Mono")

(defun my-set-font-default (font size height)
  "combine multiple set font together for easy change once."
  (interactive)
  (set-default-font (concat font "-" (number-to-string size)))
  (set-frame-font (concat font "-" (number-to-string size)) t)
  (set-face-attribute 'default nil :font font :height height)
  (set-face-font 'default font)
  )

(my-set-font-default "DejaVu Sans Mono" 10 100)

;; (set-default-font "-*-Hack-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1")

;; -------
;; Unicode
;; -------
;;
;; set unicode symbol fallback font to "Symbola".
;; (set-fontset-font "fontset-default" nil
;;                   (font-spec :size 20 :name "Symbola"))
(set-fontset-font t 'unicode
                  (font-spec :name "Symbola") nil 'append)
;; (set-fontset-font t 'unicode
;;                   (font-spec :name "Symbola") nil 'prepend)

;; greek characters
(set-fontset-font t 'greek (font-spec :name "DejaVu Sans Mono") nil)

;; override font for cyrillic characters
(set-fontset-font t 'cyrillic "Droid Sans Mono")

;; Averia-12
;; Averia Serif-12
;; Linux Libertine-13

;; -------------
;; Chinese Font
;; -------------
;;
;; other charsets except 'han : kana, symbol, cjk-misc, bopomofo,
;; - WenQuanYi Micro Hei Mono :: 文泉驿 微米黑
;; - FZSuXinShiLiuKaiS-R-GB :: 方正苏新诗柳字体
;; - DFPShaoNvW5-GB :: 华康少女体 W5
;; - DFPWaWaW5-GB :: 华康娃娃体
;; - iWawa :: 华康娃娃体
;; - FZMiaoWuS-GB :: 方正喵呜体
;;
;; 1.
;; (set-fontset-font t 'han (font-spec :family "WenQuanYi Micro Hei Mono" :size 13))
;; 2.
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset
                    (font-spec :family "WenQuanYi Micro Hei Mono" :size 13)
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



(provide 'init-my-emacs-font)

;;; init-my-emacs-font.el ends here
