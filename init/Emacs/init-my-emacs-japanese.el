;;; init-my-emacs-japanese.el --- init for Japanese support
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ migemo ] -- provides Japanese increment search with 'Romanization of Japanese'(ローマ字).

(require 'migemo)
;;; Usage:
;;
;; -

(setq migemo-command "cmigemo"
      migemo-options '("-q" "--emacs")
      ;; Set your installed path
      ;; migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict"
      ;; migemo-user-dictionary nil
      ;; migemo-regex-dictionary nil
      migemo-coding-system 'utf-8-unix
      )

(migemo-init)


;;; [ helm-migemo ] --

(require 'helm-migeomo)


;;; [ oniisama ]

;; (require 'oniisama)


(provide 'init-my-emacs-japanese)

;;; init-my-emacs-japanese.el ends here
