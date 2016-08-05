;;; init-my-language-chinese.el --- init for Chinese
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ pinyin-search ] --

(use-package pinyin-search
  :ensure t
  :config
  (define-key my-search-language-prefix (kbd "c") 'pinyin-search)
  (define-key my-search-language-prefix (kbd "C") 'pinyin-search-backward)
  )


(provide 'init-my-language-chinese)

;;; init-my-language-chinese.el ends here
