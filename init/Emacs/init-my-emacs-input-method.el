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



;;; [ Chinese Input Method ]


;;; [ chinese-pyim ]

(use-package chinese-pyim
  :ensure t
  :bind
  (("M-j" . pyim-convert-pinyin-at-point)
   ;; ("C-;" . pyim-delete-word-from-personal-buffer)
   )
  
  :config
  (setq default-input-method "chinese-pyim")
  ;; use double pinyin
  (setq pyim-default-pinyin-scheme 'default)
  
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template))
  
  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))

  ;; enable pinyin search functionality
  (setq pyim-isearch-enable-pinyin-search t)
  
  (setq pyim-use-tooltip 'pos-tip
        pyim-page-length 5)

  ;; for company-mode complete chinese
  (require 'chinese-pyim-company)
  (setq pyim-company-max-length 6)
  ;; disable company-mode completion for chinese
  ;; (setq pyim-company-complete-chinese-enable nil)
  )


;;; [ chinese-pyim-basedict ]

(use-package chinese-pyim-basedict
  :ensure t
  :config
  (chinese-pyim-basedict-enable))


;;; [ chinese-pyim-greatdict ]

;; (use-package chinese-pyim-greatdict
;;   :ensure t
;;   :config
;;   (chinese-pyim-greatdict-enable))


;;; [ TeX Input Method ]

;; - [C-u C-\ TeX RET]

;; (let ((quail-current-package (assoc "TeX" quail-package-alist)))
;;   (quail-define-rules ((append . t))
;;                       ("^\\alpha" ?ᵅ)
;;                       ))


;;; [ quail ] -- Emacs default input method which use keymap to lookup chinese.



(provide 'init-my-emacs-input-method)

;;; init-my-emacs-input-method.el ends here
