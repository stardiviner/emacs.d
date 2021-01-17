;;; init-emacs-font.el --- init for Emacs font settings -*- lexical-binding: t; -*-

;;; Commentary:

;; the value is in 1/10pt, so 100 will give you 10pt, etc
;;
;; - [C-u C-x =]
;; - [M-x describe-font]
;; - [M-x describe-fontset]
;; - ‘font-family-list’
;; - from command: $ fc-list
;;
;; Emacs set font functions:
;;
;; - `set-face-attribute'
;; - `set-frame-font'
;;
;; - "DejaVu Sans Mono"
;; - "Hack"
;; - "Fira Sans"
;; - "Sarasa Mono SC"
;; - "Sarasa Nerd"
;; - "Noto Sans CJK SC"
;; - "Noto Sans Mono CJK SC"

;;; Code:

;;; [ set font for Emacs ]

;; (cond
;;  ((x-list-fonts "DejaVu Sans Mono")
;;   (set-face-attribute 'default nil
;;                       :family "DejaVu Sans Mono")))


;; (set-fontset-font nil 'unicode
;;                   (font-spec :name "Symbola") nil 'append)

;;; [ display Unicode Emoji ]
;;; A font that supports emoji is needed. The best results are obtained with
;;; "Noto Color Emoji" or "Symbola". It might be necessary to instruct Emacs to
;;; use such font with a line like the following.
(set-fontset-font t 'symbol
                  (font-spec :family "Noto Color Emoji") nil 'prepend)

(dolist (charset '(greek
                   ;; symbol
                   ))
  (set-fontset-font nil charset
                    (font-spec :name "Symbola") nil 'prepend))

(dolist (charset '(kana han cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font) charset
                    (font-spec :family "Source Han Sans CN") nil 'append))

;;; setting default font
(set-face-attribute 'default nil :family "DejaVu Sans Mono")



(provide 'init-emacs-font)

;;; init-emacs-font.el ends here
