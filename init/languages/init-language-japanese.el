;;; init-language-japanese.el --- init for Japanese support
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ migemo ] -- provides Japanese increment search with 'Romanization of Japanese'(ローマ字).

(use-package migemo
  :ensure t
  :defer t
  :commands (migemo migemo-init)
  :custom ((migemo-command "cmigemo")
           (migemo-options '("-q" "--emacs")))
  :bind (:map language-search-prefix ("j" . migemo-isearch-toggle-migemo))
  :init (migemo-init))

(use-package ivy-migemo
  :ensure t
  :after ivy
  :init
  ;; Toggle migemo and fuzzy by command.
  (define-key ivy-minibuffer-map (kbd "M-f") #'ivy-migemo-toggle-fuzzy)
  (define-key ivy-minibuffer-map (kbd "M-m") #'ivy-migemo-toggle-migemo)
  ;; :config
  ;; ;; If you want to defaultly use migemo on swiper and counsel-find-file:
  ;; (setq ivy-re-builders-alist '((t . ivy--regex-plus)
  ;;                               (swiper . ivy-migemo--regex-plus)
  ;;                               (counsel-find-file . ivy-migemo--regex-plus))
  ;;       ;;(counsel-other-function . ivy-migemo--regex-plus)
  ;;       )
  ;; ;; Or you prefer fuzzy match like ido:
  ;; (setq ivy-re-builders-alist '((t . ivy--regex-plus)
  ;;                               (swiper . ivy-migemo--regex-fuzzy)
  ;;                               (counsel-find-file . ivy-migemo--regex-fuzzy))
  ;;       ;;(counsel-other-function . ivy-migemo--regex-fuzzy)
  ;;       )
  )

;;; [ oniisama ]

;; (require 'oniisama)

;;; [ kana ] -- Learn Japanese kana (仮名，五十音) in Emacs.

(use-package kana
  :ensure t
  :commands (kana))


(provide 'init-language-japanese)

;;; init-language-japanese.el ends here
