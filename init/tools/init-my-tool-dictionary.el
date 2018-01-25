;;; init-my-tool-dictionary.el --- init my Emacs dictionary.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'dictionary-prefix)
  (define-prefix-command 'dictionary-prefix))
(define-key tools-prefix (kbd "d") 'dictionary-prefix)

;;; [ sdcv.el ]

(use-package sdcv
  :ensure t
  :bind (:map dictionary-prefix
              ("C-d" . sdcv-search-pointer+)
              ("M-d" . sdcv-search-input))
  :config
  ;; a simple dictionary list for popup display
  (setq sdcv-dictionary-simple-list
        '(;; "WordNet"
          "牛津英汉双解美化版"
          ))
  ;; a complete dictionary list for buffer display
  (setq sdcv-dictionary-complete-list
        '("WordNet"
          "牛津英汉双解美化版"
          "朗道英汉字典5.0"
          "朗道汉英字典5.0"
          ))
  )

;;; [ Goldendict ]

(use-package goldendict
  :ensure t
  :bind (:map dictionary-prefix
              ("d" . goldendict-dwim)))

;;; [ google-translate ]

;; (use-package google-translate
;;   :ensure t
;;   :defer t
;;   :bind (:map dictionary-prefix
;;               ("t" . google-translate-smooth-translate)
;;               ("T" . google-translate-at-point)
;;               ("C-t" . google-translate-query-translate)
;;               )
;;   :config
;;   (setq google-translate-enable-ido-completion nil
;;         google-translate-show-phonetic t
;;         ;; google-translate-listen-program
;;         google-translate-output-destination nil ; 'echo-area, 'popup
;;         google-translate-pop-up-buffer-set-focus nil
;;         )
;;   )

;;; [ ob-translate ] -- allows you to translate blocks of text within org-mode.

;; (use-package ob-translate
;;   :ensure t
;;   :init
;;   ;; translate special block
;;   (add-to-list 'org-structure-template-alist '(?t . "translate"))
;;   )


(provide 'init-my-tool-dictionary)

;;; init-my-tool-dictionary.el ends here
