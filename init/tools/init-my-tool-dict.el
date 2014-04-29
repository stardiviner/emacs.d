;;; init-my-tool-dict.el --- init my Emacs dictionary.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(load "~/.emacs.d/init/extensions/sdcv.el")


(setq sdcv-dictionary-simple-list ;; a simple dictionary list for popup display
      '(;; "WordNet"
        "牛津英汉双解美化版"
        ))

(setq sdcv-dictionary-complete-list ;; a complete dictionary list for buffer display
      '("WordNet"
        "牛津英汉双解美化版"
        "朗道英汉字典5.0"
        "朗道汉英字典5.0"
        ))

(unless (boundp 'dictionary-map)
  (define-prefix-command 'dictionary-map))
(define-key my-tools-prefix-map (kbd "d") 'dictionary-map)

(define-key dictionary-map (kbd "d") 'sdcv-search-pointer+)
(define-key dictionary-map (kbd "C-d") 'sdcv-search-input)



(provide 'init-my-tool-dict)

;;; init-my-tool-dict.el ends here
