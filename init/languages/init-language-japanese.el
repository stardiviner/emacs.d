;;; init-language-japanese.el --- init for Japanese support
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ migemo ] -- provides Japanese increment search with 'Romanization of Japanese'(ローマ字).

(use-package migemo
  :ensure t
  :defer t
  :commands (migemo)
  :bind (:map language-search-prefix
              ("j" . migemo-isearch-toggle-migemo))
  :config
  (setq migemo-command "cmigemo"
        migemo-options '("-q" "--emacs")
        ;; Set your installed path
        ;; migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict"
        ;; migemo-user-dictionary nil
        ;; migemo-regex-dictionary nil
        migemo-coding-system 'utf-8-unix
        )

  ;; (migemo-init)
  )


;;; [ oniisama ]

;; (require 'oniisama)

;;; [ kana ] -- Learn Japanese kana (仮名，五十音) in Emacs.

(use-package kana
  :quelpa (kana :repo "chenyanming/kana" :fetcher github)
  :commands (kana))


(provide 'init-language-japanese)

;;; init-language-japanese.el ends here
