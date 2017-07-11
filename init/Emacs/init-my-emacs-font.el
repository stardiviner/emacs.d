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
;; ------- Handwriting fonts -----
;; Comic Sans MS
;; Comic Neue
;; Gabriola
;; Old London
;; Segoe (Print/Script)
;; Death Note
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

;; (set-default-font "-*-Hack-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1")
;; (my-set-font-default "Hack" 10 100)

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

(defun my-default-fonts-setup (default-height variable-pitch-height)
  "Set up default fonts.

Use DEFAULT-HEIGHT for default face and VARIABLE-PITCH-HEIGHT
for variable-pitch face."
  (set-face-attribute 'default nil
                      :family "DejaVu Sans Mono"
                      :height default-height)
  (set-face-attribute 'variable-pitch nil
                      :family "Monospace"
                      :height variable-pitch-height
                      :weight 'normal))

(when window-system
  (if (> (x-display-pixel-width) 1800)
      (my-default-fonts-setup 110 60)
    (my-default-fonts-setup 90 50)))

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
(set-fontset-font t 'greek (font-spec :name "DejaVu Sans Mono") nil)

;; override font for cyrillic characters
(set-fontset-font t 'cyrillic "Droid Sans Mono")

;; -------------
;; CJK (Chinese, Japanese, Korean)
;; -------------
;;
;; other charsets except 'han : kana, symbol, cjk-misc, bopomofo,
;; - WenQuanYi Micro Hei Mono :: 文泉驿 微米黑
;; - "Hiragino Sans GB W3"
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
                    ;; (font-spec :family "Noto Sans CJK SC" :size 12)
                    (font-spec :family "WenQuanYi Micro Hei" :size 13)
                    ;; (font-spec :family "HanaMinA" :size 14)
                    ))

;;; need to modify English font settings to suitable with chinese font.
;;; the value is in 1/10pt, so 100 will give you 10pt, etc



;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (set-frame-font (format "%s:pixelsize=%d" "DejaVu Sans Mono" 12) t)))


;;; [ font-lock-profiler ] -- Coverage and timing tool for font-lock keywords.

;; (use-package font-lock-profiler
;;   :ensure t)

;;; [ font-lock-studio ] -- Interactive debugger for font-lock keywords (Emacs syntax highlighting rules).

;; (use-package font-lock-studio
;;   :ensure t)


;;; font size config for Org-mode table chinese & english mixture.
(when (window-system)
  (defvar emacs-english-font "DejaVu Sans Mono"
    "The font name of English.")

  (defvar emacs-cjk-font "文泉驛等寬微米黑" "The font name for CJK.")
  (setq emacs-cjk-font "WenQuanYi Micro Hei Mono")

  (defvar emacs-font-size-pair '(12 . 14)
    "Default font size pair for (english . chinese)")

  (defvar emacs-font-size-pair-list
    '(( 5 .  6) (9 . 10) (10 . 12)(12 . 14)
      (13 . 16) (15 . 18) (17 . 20) (19 . 22)
      (20 . 24) (21 . 26) (24 . 28) (26 . 32)
      (28 . 34) (30 . 36) (34 . 40) (36 . 44))
    "This list is used to store matching (englis . chinese) font-size.")

  (defun font-exist-p (fontname)
    "Test if this font is exist or not."
    (if (or (not fontname) (string= fontname ""))
        nil
      (if (not (x-list-fonts fontname)) nil t)))

  (defun set-font (english chinese size-pair)
    "Setup emacs English and Chinese font on x window-system."

    (if (font-exist-p english)
        (set-frame-font (format "%s:pixelsize=%d" english (car size-pair)) t))

    (if (font-exist-p chinese)
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font) charset
                            (font-spec :family chinese :size (cdr size-pair))))))
  ;; Setup font size based on emacs-font-size-pair
  (set-font emacs-english-font emacs-cjk-font emacs-font-size-pair)

  (defun emacs-step-font-size (step)
    "Increase/Decrease emacs's font size."
    (let ((scale-steps emacs-font-size-pair-list))
      (if (< step 0) (setq scale-steps (reverse scale-steps)))
      (setq emacs-font-size-pair
            (or (cadr (member emacs-font-size-pair scale-steps))
                emacs-font-size-pair))
      (when emacs-font-size-pair
        (message "emacs font size set to %.1f" (car emacs-font-size-pair))
        (set-font emacs-english-font emacs-cjk-font emacs-font-size-pair))))

  (defun increase-emacs-font-size ()
    "Decrease emacs's font-size acording emacs-font-size-pair-list."
    (interactive) (emacs-step-font-size 1))

  (defun decrease-emacs-font-size ()
    "Increase emacs's font-size acording emacs-font-size-pair-list."
    (interactive) (emacs-step-font-size -1))

  (global-set-key (kbd "C-+") 'increase-emacs-font-size)
  (global-set-key (kbd "C--") 'decrease-emacs-font-size)
  )



(provide 'init-my-emacs-font)

;;; init-my-emacs-font.el ends here
