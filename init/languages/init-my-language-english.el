;;; init-my-language-english.el --- init for English Language.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ capitalized-words-mode ]

;; (capitalized-words-mode 1)


;;; [ auto-capitalize ]

;; (use-package auto-capitalize
;;   :ensure t
;;   :init
;;   (autoload 'auto-capitalize "auto-capitalize"
;;     "Autoload function `auto-capitalize'." t)
;;   (autoload 'auto-capitalize-mode "auto-capitalize"
;;     "Toggle `auto-capitalize' minor mode in the buffer." t)
;;   (autoload 'turn-on-auto-capitalize-mode "auto-capitalize"
;;     "Turn on `auto-capitalize' minor mode in the buffer." t)
;;   (autoload 'enable-auto-capitalize-mode "auto-capitalize"
;;     "Enable `auto-capitalize' minor mode in the buffer." t)
;;   :config
;;   (setq auto-capitalize-yank nil
;;         auto-capitalize-words '("\\<Emacs\\>" "\\<Linux\\>"
;;                                 "\\<Android\>>" "\\<iOS\\>" "\\<Mac\\>")
;;         ;; TODO: auto-capitalize-predicate
;;         )
;;
;;   ;; To turn on (unconditional) capitalization in all Text modes.
;;   ;; (turn-on-auto-capitalize-mode)
;;   ;; To enable (interactive) capitalization in all Text modes.
;;   ;; (enable-auto-capitalize-mode)
;;   ;; enable in specific modes.
;;   (dolist (hook '(text-mode-hook
;;                   org-mode-hook
;;                   markdown-mode-hook
;;                   ;; latex-mode-hook
;;                   ))
;;     (add-hook hook (lambda ()
;;                      (turn-on-auto-capitalize-mode))))
;;   )


;;; [ Super Smart Capitalization ]

;; Languages are fleeting. But Emacs is forever.
;; =>
;; Languages are fleeting, but Emacs is forever.

(global-set-key "\M-c" 'my/capitalize)
(global-set-key "\M-l" 'my/downcase)
(global-set-key "\M-u" 'my/upcase)

(defun my/convert-punctuation (rg rp)
  "Look for regexp RG around point, and replace with RP.
Only applies to text-mode."
  (let ((f "\\(%s\\)\\(%s\\)")
        (space "?:[[:blank:]\n\r]*"))
    ;; We obviously don't want to do this in prog-mode.
    (if (and (derived-mode-p 'text-mode)
             (or (looking-at (format f space rg))
                 (looking-back (format f rg space))))
        (replace-match rp nil nil nil 1))))

(defun my/capitalize ()
  "Capitalize region or word.
Also converts commas to full stops, and kills
extraneous space at beginning of line."
  (interactive)
  (my/convert-punctuation "," ".")
  (if (use-region-p)
      (call-interactively 'capitalize-region)
    ;; A single space at the start of a line:
    (when (looking-at "^\\s-\\b")
      ;; get rid of it!
      (delete-char 1))
    (call-interactively 'subword-capitalize)))

(defun my/downcase ()
  "Downcase region or word.
Also converts full stops to commas."
  (interactive)
  (my/convert-punctuation "\\." ",")
  (if (use-region-p)
      (call-interactively 'downcase-region)
    (call-interactively 'subword-downcase)))

(defun my/upcase ()
  "Upcase region or word."
  (interactive)
  (if (use-region-p)
      (call-interactively 'upcase-region)
    (call-interactively 'subword-upcase)))


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

;; (use-package predictive
;;   :ensure t
;;   :config
;;   )

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
