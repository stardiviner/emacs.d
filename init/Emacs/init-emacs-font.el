;;; init-emacs-font.el --- init for Emacs font settings -*- lexical-binding: t; -*-

;;; Time-stamp: <2020-12-10 12:47:32 stardiviner>

;;; Commentary:



;;; Code:

;;; [ set font for Emacs ]

;; (cond
;;  ((x-list-fonts "DejaVu Sans Mono")
;;   (set-face-attribute 'default nil
;;                       :family "DejaVu Sans Mono")))


;;; [ display Unicode Emoji ]

;;; A font that supports emoji is needed. The best results are obtained with
;;; "Noto Color Emoji" or "Symbola". It might be necessary to instruct Emacs to
;;; use such font with a line like the following.
(set-fontset-font t 'symbol
                  (font-spec :family "Noto Color Emoji") nil 'prepend)



(provide 'init-emacs-font)

;;; init-emacs-font.el ends here
