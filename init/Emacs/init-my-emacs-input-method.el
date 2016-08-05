;;; init-my-emacs-input-method.el --- init Input Method
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;; [ Input Method ]

;; Usage:
;; - [C-h I] / [C-u C-\] + [METHOD] -- describe the input method
;; - [C-u C-\] -- interactively choose input method
;; - [C-x Return C-\]
;; - [M-x set-input-method]
;;   - Chinese-PY
;; - [C-\] -- toggle-input-method
;; - [C-h C-\ METHOD] -- describe the input method
;; - [C-u C-x =] -- check out how to input the character after point using current input method.
;; - chinese-py :: chinese pinyin.
;; - greek :: Greek.
;; - keys:
;;   - [C-f/b] -- forward/backward
;;   - [C-n/p] -- next/previous
;; * compose key
;; * ucs-insert
;; - [C-x 8 RET]

(setq default-input-method "TeX") ; default: "rfc1345", "TeX", "chinese-py",

(setq input-method-verbose-flag t)
;; (global-set-key (kbd "C-SPC") 'nil) ; disable [C-SPC] for input method.


;;; [ TeX Input Method ]

;; - [C-u C-\ TeX RET]

;; (let ((quail-current-package (assoc "TeX" quail-package-alist)))
;;   (quail-define-rules ((append . t))
;;                       ("^\\alpha" ?ᵅ)
;;                       ))



;;; [ Chinese Input Method ]

;; (require 'init-my-emacs-input-method-chinese)


(provide 'init-my-emacs-input-method)

;;; init-my-emacs-input-method.el ends here
