;;; init-my-language-japanese.el --- init for Japanese support
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ migemo ] -- provides Japanese increment search with 'Romanization of Japanese'(ローマ字).

(use-package migemo
  :ensure t
  :config
  (autoload 'migemo "migemo" "" t)

  (setq migemo-command "cmigemo"
        migemo-options '("-q" "--emacs")
        ;; Set your installed path
        ;; migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict"
        ;; migemo-user-dictionary nil
        ;; migemo-regex-dictionary nil
        migemo-coding-system 'utf-8-unix
        )

  (migemo-init)
  )


;;; [ oniisama ]

;; (require 'oniisama)


(provide 'init-my-language-japanese)

;;; init-my-language-japanese.el ends here
