;;; init-language-japanese.el --- init for Japanese support
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ migemo ] -- provides Japanese increment search with 'Romanization of Japanese'(ローマ字).

(use-package migemo
  :ensure t
  :defer t
  :commands (migemo)
  :custom ((migemo-command "cmigemo")
           (migemo-options '("-q" "--emacs")))
  :bind (:map language-search-prefix
              ("j" . migemo-isearch-toggle-migemo))
  ;; :init (migemo-init)
  )

;;; [ oniisama ]

;; (require 'oniisama)

;;; [ kana ] -- Learn Japanese kana (仮名，五十音) in Emacs.

(use-package kana
  :ensure t
  :commands (kana))


(provide 'init-language-japanese)

;;; init-language-japanese.el ends here
