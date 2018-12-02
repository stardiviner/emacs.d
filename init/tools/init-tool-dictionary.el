;;; init-tool-dictionary.el --- init my Emacs dictionary.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'dictionary-prefix)
  (define-prefix-command 'dictionary-prefix))
(define-key tools-prefix (kbd "d") 'dictionary-prefix)

;;; [ sdcv.el ]

;; (use-package sdcv
;;   :ensure t
;;   :commands (sdcv-search-pointer+ sdcv-search-input)
;;   :bind (:map dictionary-prefix
;;               ("C-d" . sdcv-search-pointer+)
;;               ("M-d" . sdcv-search-input))
;;   :config
;;   ;; a simple dictionary list for popup display
;;   (setq sdcv-dictionary-simple-list
;;         '("懒虫简明英汉词典"
;;           "懒虫简明汉英词典"
;;           ;; "WordNet"
;;           "牛津英汉双解美化版"))
;;   ;; a complete dictionary list for buffer display
;;   (setq sdcv-dictionary-complete-list
;;         '("WordNet"
;;           "牛津英汉双解美化版"
;;           "朗道英汉字典5.0"
;;           "朗道汉英字典5.0")))

;;; [ manateelazycat/sdcv ]

(use-package sdcv
  :quelpa (sdcv :fetcher github :repo "manateelazycat/sdcv")
  :ensure posframe
  :commands (sdcv-search-pointer sdcv-search-pointer+ sdcv-search-input sdcv-search-input+)
  :bind (:map dictionary-prefix
              ("C-d" . sdcv-search-pointer+)
              ("M-d" . sdcv-search-input))
  :init
  (setq sdcv-say-word-p t)
  ;; a simple dictionary list for popup display
  (setq sdcv-dictionary-simple-list
        '(;; "懒虫简明英汉词典"
          ;; "懒虫简明汉英词典"
          ;; "WordNet"
          "牛津英汉双解美化版"))
  ;; a complete dictionary list for buffer display
  (setq sdcv-dictionary-complete-list
        '("WordNet"
          "牛津英汉双解美化版"
          "朗道英汉字典5.0"
          "朗道汉英字典5.0")))

;;; [ Goldendict ]

(use-package goldendict
  :ensure t
  :bind (:map dictionary-prefix ("d" . goldendict-dwim)))

;;; [ google-translate ]

(use-package google-translate
  :ensure t
  :defer t
  :bind (:map dictionary-prefix
              ("t" . google-translate-smooth-translate)
              ("C-t" . google-translate-at-point)
              ("M-t" . google-translate-query-translate)
              ("C-r" . google-translate-at-point-reverse)
              ("M-r" . google-translate-query-translate-reverse))
  :config
  (setq google-translate-enable-ido-completion nil
        google-translate-show-phonetic t
        ;; google-translate-listen-program
        google-translate-output-destination nil ; 'echo-area, 'popup
        google-translate-pop-up-buffer-set-focus t
        ;; `google-translate-supported-languages'
        google-translate-default-target-language "zh-CN"
        ;; for `google-translate-smooth-translate' + [C-n/p]
        google-translate-translation-directions-alist '(("en" . "zh-CN")
                                                        ("zh-CN" . "en")
                                                        ("zh-CN" . "ja")
                                                        ("zh-CN" . "ko"))
        )
  
  (add-to-list 'display-buffer-alist
               '("^\\*Google Translate\\*" .
                 (display-buffer-reuse-window display-buffer-below-selected)))
  )

;;; [ ob-translate ] -- allows you to translate blocks of text within org-mode.

(use-package ob-translate
  :ensure t
  :config
  (add-to-list 'org-babel-load-languages '(translate . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  ;; add translate special block into structure template alist.
  (add-to-list 'org-structure-template-alist '("t" . "src translate"))
  (defun ob-translate-toggle-proxy (origin-func body params)
    (call-interactively 'proxy-mode-enable)
    (let ((output (funcall origin-func body params)))
      (call-interactively 'proxy-mode-disable)
      output))  
  (advice-add 'org-babel-execute:translate :around #'ob-translate-toggle-proxy))

;;; [ youdao-dictionary ] -- Youdao Dictionary (有道词典) interface for Emacs

(use-package youdao-dictionary
  :ensure t
  :bind (:map dictionary-prefix ("y" . youdao-dictionary-search-at-point))
  :init
  (add-to-list 'display-buffer-alist
               '("^\\*Youdao Dictionary\\*" .
                 (display-buffer-reuse-window display-buffer-below-selected))))


(provide 'init-tool-dictionary)

;;; init-tool-dictionary.el ends here
