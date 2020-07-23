;;; init-emacs-input-method.el --- init Input Method
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

;;; [ pyim ] -- 一个 emacs 中文输入法，支持全拼，双拼和五笔.

;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
;; 我自己使用的中英文动态切换规则是：
;; 1. 光标只有在注释里面时，才可以输入中文。
;; 2. 光标前是汉字字符时，才能输入中文。
;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。

;; (use-package pyim
;;   :ensure t
;;   :init (setq default-input-method "pyim")
;;   :config
;;   (require 'pyim-basedict) ; 拼音词库设置，五笔用户 *不需要* 此行设置
;;   (pyim-basedict-enable)   ; 拼音词库，五笔用户 *不需要* 此行设置
;;
;;   ;; (setq pyim-page-tooltip 'posframe
;;   ;;       pyim-page-style 'two-lines
;;   ;;       pyim-page-length 5)
;;
;;   ;; (setq-default pyim-english-input-switch-functions
;;   ;;               '(pyim-probe-dynamic-english
;;   ;;                 pyim-probe-isearch-mode
;;   ;;                 pyim-probe-program-mode
;;   ;;                 pyim-probe-org-structure-template))
;;   ;; (setq-default pyim-punctuation-half-width-functions
;;   ;;               '(pyim-probe-punctuation-line-beginning
;;   ;;                 pyim-probe-punctuation-after-punctuation))
;;   )

;;; [ emacs-rime ] -- RIME ㄓ in Emacs using librime.

(use-package rime
  :ensure t
  :custom ((rime-user-data-dir "~/.config/fcitx/rime")
           (default-input-method "rime")
           (rime-show-candidate 'posframe)
           (rime-cursor "˰")
           (rime-posframe-properties (list :background-color "#333333"
                                           :foreground-color "#dcdccc"
                                           :internal-border-width 10)))
  :config (if (eq rime-show-candidate 'posframe)
              (setq rime-posframe-style 'vertical)))



(provide 'init-emacs-input-method)

;;; init-emacs-input-method.el ends here
