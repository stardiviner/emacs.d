;;; init-my-emacs-prettify.el --- init for prettify Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


;;; [ prettify-symbols-mode ]

;; TODO:
;; (setq prettify-symbols-alist '(("lambda" . 955)))

;; (global-prettify-symbols-mode 1)


;;; [ pretty-mode ] -- redisplay parts of the Emacs buffer as pretty symbols.

(require 'pretty-mode)

(add-to-list 'pretty-supported-modes 'ruby-mode)
(add-to-list 'pretty-supported-modes 'enh-ruby-mode)

;; TODO:
;; (add-to-list 'pretty-default-groups '(:function))
;; (add-to-list 'pretty-active-groups '(ruby-mode :function))
;; (add-to-list 'pretty-active-patterns '(ruby-mode))
;; (add-to-list 'pretty-patterns '((ruby-mode ("->" . ?λ))))

;;; 1. if you want to set it globally
;; (global-pretty-mode t)
;;; 2. if you want to set it only for a specific mode
;; (dolist (hook '(prog-mode-hook
;;                 ))
;;   (add-hook hook 'turn-on-pretty-mode))



;;; [ pretty-symbols ]

(use-package pretty-symbols
  :config
  (setq pretty-symbol-categories '(lambda relational logical)
        ;; pretty-symbol-patterns '()
        )

  (global-prettify-symbols-mode 1)
  )


;;; [ page-break-lines-mode ] --- page breaks (^L characters) are displayed as a horizontal line of a character.

;;; In Page Break mode, page breaks (^L characters) are displayed as a horizontal line of `page-break-string-char' characters.

(require 'page-break-lines)

(global-page-break-lines-mode t)

(setq page-break-lines-char ?─)


(provide 'init-my-emacs-prettify)

;;; init-my-emacs-prettify.el ends here
