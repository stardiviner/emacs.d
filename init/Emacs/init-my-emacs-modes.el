;;; init-my-emacs-modes.el --- init Emacs modes settings
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ mmm-mode ] -- Minor mode to allow multiple major modes in one buffer.

;;; Usage:
;; prefix
;; - [C-c % PUNCTUATION] -- prefix
;;
;; 1. [C-c % C-c] (`mmm-ify-by-class')
;; a better way: (auto add buffer submode regions)
;; to add submode regions by using submode classes.
;; - [M-x mmm-mode] to enable mmm-mode in this buffer.
;;  then [C-c % C-c], and enter the name of the class to add submode regions automatically.
;;
;; 2. [C-c % C-r] (`mmm-ify-region')
;; quick start: (mark area submode region)
;; - [M-x mmm-mode] to mark the area you want to make into a submode region.
;;  then [C-c % C-r], and enter the desired major mode.
;;
;; 3. [C-c % C-x] (`mmm-ify-by-regexp')
;;   scans the buffer for submode regions.
;;
;; 4. apply submode class to all files which matches conditions.
;;    (mmm-add-mode-ext-class MODE EXTENSION CLASS)


(require 'mmm-mode)

(setq mmm-global-mode 'maybe) ; t, nil, 'maybe (turn itself on in precisely).

(setq mmm-submode-mode-line-format "~M>[~m]")

(setq mmm-submode-decoration-level 3)


;;; submode classes

;; (mmm-add-classes
;;  '((embedded-css
;;     :submode css
;;     :face mmm-declaration-submode-face
;;     :front "<style[^>]*>"
;;     :back "</style>")))


;;; submode groups

;; (mmm-add-to-group 'html-js '((js-html
;;                               :submode javascript
;;                               :face mmm-code-submode-face
;;                               :front "%=%"
;;                               :back "%=%"
;;                               :end-not-begin t)))


;;; [ mumamo-noweb ] -- multiple major modes




;;; [ auto-mode-alist ]

;;; setup some auto-mode-alist
(add-to-list 'auto-mode-alist
             ;; Conky
             '("\\.conkyrc$" . conf-mode)
             '("conkyrc$" . conf-mode))


(provide 'init-my-emacs-modes)

;;; init-my-emacs-modes.el ends here
