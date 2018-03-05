;;; init-language-chinese.el --- init for Chinese
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ chinese-word-at-point ] -- Get (most likely) Chinese word under the cursor in Emacs.

(use-package chinese-word-at-point
  :ensure t)

;;; [ pinyinlib ] -- Elisp library for converting first letter of Pinyin to Simplified/Traditional Chinese characters.

(use-package pinyinlib
  :ensure t)

;;; [ pinyin-search ] -- Search Chinese by Pinyin

(use-package pinyin-search
  :ensure t
  :bind (:map language-search-prefix
              ("c" . pinyin-search)
              ("C" . pinyin-search-backward))
  )


(provide 'init-language-chinese)

;;; init-language-chinese.el ends here