;;; init-my-language-english.el --- init for English Language.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ capitalized-words-mode ]

;; (capitalized-words-mode 1)


;;; [ auto-capitalize ]

;;; Usage:
;;
;; - auto capitalize words of an sentence.
;; - prevent a word in the `auto-capitalize-words' list from being capitalized or upcased in a particular context.
;;   e.g. ("GNU.emacs.sources"), insert the following whitepsace or punctuation character with:
;;   `M-x quoted insert' (e.g. `gnu C-q .').

;; (autoload 'auto-capitalize "auto-capitalize" "Autoload function `auto-capitalize'." t)
;; (autoload 'auto-capitalize-mode "auto-capitalize" "Toggle `auto-capitalize' minor mode in the buffer." t)
;; (autoload 'turn-on-auto-capitalize-mode "auto-capitalize" "Turn on `auto-capitalize' minor mode in the buffer." t)
;; (autoload 'enable-auto-capitalize-mode "auto-capitalize" "Enable `auto-capitalize' minor mode in the buffer." t)

;; (setq auto-capitalize-yank nil
;;       auto-capitalize-words '("\\<Emacs\\>" "\\<Linux\\>"
;;                               "\\<Android\>>" "\\<iOS\\>" "\\<Mac\\>")
;;       ;; TODO: auto-capitalize-predicate
;;       )

;;; To turn on (unconditional) capitalization in all Text modes.
;; (turn-on-auto-capitalize-mode)
;;; To enable (interactive) capitalization in all Text modes.
;; (enable-auto-capitalize-mode)

;; (dolist (hook '(text-mode-hook
;;                 org-mode-hook
;;                 markdown-mode-hook
;;                 ;; latex-mode-hook
;;                 ))
;;   (add-hook hook (lambda ()
;;                    (turn-on-auto-capitalize-mode))))


;;; [ electric punctuation ]

;;; auto insert space after punctuation.
;;
;; (defun my-electric-punctuation ()
;;   "Tidy up whitespace around punctuation: delete any preceding
;;   whitespace and insert one space afterwards.  Idea stolen from
;;   the SwiftKey android keyboard."
;;   (interactive)
;;   (when (looking-back "\s+" nil t)
;;     (delete-region (match-beginning 0) (match-end 0)))
;;   (call-interactively 'self-insert-command)
;;   (just-one-space))
;;
;; (dolist (hook '(text-mode-hook
;;                 org-mode-hook
;;                 markdown-mode-hook))
;;   (add-hook hook
;;             '(lambda ()
;;                (dolist (punc '(?, ?\; ?.))
;;                  (define-key text-mode-map `[,punc] 'my-electric-punctuation)))))


;;; [ predictive-mode ] -- tries to predict the rest of the word, and offers you an appropriate completion.

;; (require 'predictive)

;; ;;; ### Predictive ###
;; ;;; --- 英语助手
;; (set-default 'predictive-auto-add-to-dict t) ;自动加入词典
;; (setq predictive-add-to-dict-ask nil)        ;加入词典不询问
;; (setq predictive-auto-learn t)               ;自动学习
;; (setq predictive-completion-speed 0.1)       ;查找补全的速度(秒)
;; (setq completion-auto-show-delay 0.5)        ;弹出补全tooltip的延迟(秒)
;; (dolist (hook (list
;;                'erc-mode-hook
;;                'rcirc-mode-hook
;;                'message-mode-hook
;;                'yaoddmuse-mode-hook
;;                ))
;;   (add-hook hook '(lambda () (predictive-mode 1))))

;; (lazy-unset-key
;;  '("TAB")
;;  completion-dynamic-map)                ;卸载按键
;; (lazy-set-key
;;  '(
;;    ("M-h" . completion-accept)          ;接受辅助补全
;;    ("M-H" . completion-reject)          ;拒绝辅助补全
;;    )
;;  completion-map
;;  )



(provide 'init-my-language-english)

;;; init-my-language-english.el ends here
