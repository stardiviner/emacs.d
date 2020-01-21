;;; init-language-chinese.el --- init for Chinese
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ pangu-spacing ] -- Emacs minor-mode to add space between Chinese and English characters.

(use-package pangu-spacing
  :ensure t
  :defer t
  :init (global-pangu-spacing-mode 1)
  (add-hook 'org-mode-hook
            '(lambda () (set (make-local-variable 'pangu-spacing-real-insert-separtor) t))))

;;; [ chinese-word-at-point ] -- Get (most likely) Chinese word under the cursor in Emacs.

(use-package chinese-word-at-point
  :ensure t
  :defer t)

;;; [ pinyinlib ] -- Elisp library for converting first letter of Pinyin to Simplified/Traditional Chinese characters.

(use-package pinyinlib
  :ensure t
  :defer t)

;;; [ pinyin-search ] -- Search Chinese by Pinyin

(use-package pinyin-search
  :ensure t
  :defer t
  :commands (pinyin-search pinyin-search-backward)
  :bind (:map language-search-prefix ("c" . pinyin-search) ("C" . pinyin-search-backward)))

;;; [ jieba.el ] -- 在Emacs中使用jieba中文分词.

;; (use-package jieba
;;   :quelpa (jieba :fetcher github :repo "cireu/jieba.el")
;;   :commands (jieba-mode)
;;   :init (jieba-mode))


(provide 'init-language-chinese)

;;; init-language-chinese.el ends here
