;;; init-my-emacs-prettify.el --- init for prettify Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ prettify-symbols-mode ]

;; TODO:
;; (setq prettify-symbols-alist '(("lambda" . 955)))

;; If you set it to t, prettify-symbols will “unprettify” a symbol as long as
;; the cursor is inside it. So you can easily (and temporarily) reveal a symbol
;; by just moving over to it.
(setq prettify-symbols-unprettify-at-point 'right-edge)

(global-prettify-symbols-mode 1)


;;; [ pretty-mode ] -- redisplay parts of the Emacs buffer as pretty symbols.

(use-package pretty-mode
  :ensure t
  :config
  (add-to-list 'pretty-supported-modes 'ruby-mode)
  (add-to-list 'pretty-supported-modes 'enh-ruby-mode)

  ;; TODO:
  ;; (add-to-list 'pretty-default-groups '(:function))
  ;; (add-to-list 'pretty-active-groups '(ruby-mode :function))
  ;; (add-to-list 'pretty-active-patterns '(ruby-mode))
  ;; (add-to-list 'pretty-patterns '((ruby-mode ("->" . ?λ))))

  ;; 1. if you want to set it globally
  ;; (global-pretty-mode t)
  ;; 2. if you want to set it only for a specific mode
  ;; (dolist (hook '(prog-mode-hook
  ;;                 ))
  ;;   (add-hook hook 'turn-on-pretty-mode))
  )


;;; [ pretty-symbols ]

(use-package pretty-symbols
  :ensure t
  :config
  ;; (setq pretty-symbol-categories '(lambda relational logical)
  ;;       ;; pretty-symbol-patterns '()
  ;;       )

  (global-prettify-symbols-mode 1)

  ;; only prettify lambda in Lisp dialects with two spaces without breaking
  ;; indentation.

  ;; [ Lisp / Clojure ]
  ;; clojure-mode already defines fn to display as λ,
  (defvar my/clojure-prettify-alist '())
  
  (add-to-list 'my/clojure-prettify-alist
               '("<=" . (?· (Br . Bl) ?≤)))
  (add-to-list 'my/clojure-prettify-alist
               '(">=" . (?· (Br . Bl) ?≥)))

  (add-to-list 'my/clojure-prettify-alist
               '("->" . (?\s (Br . Bl) ?\s (Bc . Bc) ?⇨)))
  (add-to-list 'my/clojure-prettify-alist
               '("->>" . (?\s (Br . Bl) ?\s (Br . Bl) ?\s
                              (Bc . Br) ?⇨ (Bc . Bl) ?⇨)))

  (eval-after-load 'clojure-mode
    '(setq clojure--prettify-symbols-alist
           (append my/clojure-prettify-alist
                   clojure--prettify-symbols-alist)))
  (eval-after-load 'lisp-mode
    '(setq lisp--prettify-symbols-alist
           (append my/clojure-prettify-alist
                   lisp--prettify-symbols-alist)))
  )


;;; [ page-break-lines-mode ] --- page breaks (^L characters) are displayed as a horizontal line of a character.

;;; In Page Break mode, page breaks (^L characters) are displayed as a horizontal line of `page-break-string-char' characters.

(use-package page-break-lines
  :ensure t
  :config
  (setq page-break-lines-char ?─)
  (global-page-break-lines-mode t)
  )


(provide 'init-my-emacs-prettify)

;;; init-my-emacs-prettify.el ends here
