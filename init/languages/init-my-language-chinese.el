;;; init-my-language-chinese.el --- init for Chinese
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ chinese-word-at-point ] -- Get (most likely) Chinese word under the cursor in Emacs.

(use-package chinese-word-at-point
  :ensure t)

;;; [ pinyin-search ] --

(use-package pinyin-search
  :ensure t
  :defer t
  :init
  (define-key my-search-language-prefix (kbd "c") 'pinyin-search)
  (define-key my-search-language-prefix (kbd "C") 'pinyin-search-backward)
  )


(provide 'init-my-language-chinese)

;;; init-my-language-chinese.el ends here
