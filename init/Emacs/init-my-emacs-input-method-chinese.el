;;; init-my-emacs-input-method-chinese.el --- init Input Method for Chinese.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

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

(use-package chinese-pyim-greatdict
  :ensure t
  :config
  (chinese-pyim-greatdict-enable))


;;; [ quail ] -- Emacs default input method which use keymap to lookup chinese.


(provide 'init-my-emacs-input-method-chinese)

;;; init-my-emacs-input-method-chinese.el ends here
