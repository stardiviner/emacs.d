;;; init-my-emacs-prettify.el --- init for prettify Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ pretty-symbols ]

(use-package pretty-symbols
  :ensure t
  :defer t
  :init
  (global-prettify-symbols-mode 1)
  
  :config
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  
  ;; (setq pretty-symbol-categories '(lambda relational logical)
  ;;       ;; pretty-symbol-patterns '()
  ;;       )

  ;; only prettify lambda in Lisp dialects with two spaces without breaking
  ;; indentation.

  ;; [ Lisp / Clojure ]
  ;; prettify with symbols
  ;; (defvar my/clojure-prettify-alist '())
  ;;
  ;; (add-to-list 'my/clojure-prettify-alist
  ;;              '("<=" . (?· (Br . Bl) ?≤)))
  ;; (add-to-list 'my/clojure-prettify-alist
  ;;              '(">=" . (?· (Br . Bl) ?≥)))
  ;; (add-to-list 'my/clojure-prettify-alist
  ;;              '("->" .  (?\s (Br . Bl) ?\s (Bc . Bc) ?⇨)))
  ;; (add-to-list 'my/clojure-prettify-alist
  ;;              '("->>" . (?\s (Br . Bl) ?\s (Br . Bl) ?\s
  ;;                             (Bc . Br) ?⇨ (Bc . Bl) ?⇨)))
  ;;
  ;; (with-eval-after-load 'clojure-mode
  ;;   (setq clojure--prettify-symbols-alist
  ;;         (append my/clojure-prettify-alist
  ;;                 clojure--prettify-symbols-alist)))
  ;; (with-eval-after-load 'lisp-mode
  ;;   (setq lisp-prettify-symbols-alist
  ;;         (append my/clojure-prettify-alist
  ;;                 lisp-prettify-symbols-alist)))
  )


;;; [ digit-groups ] -- highlight place-value positions in numbers.

(use-package digit-groups
  :ensure t
  :defer t
  :init
  (add-hook 'org-mode-hook #'digit-groups-mode)

  :config
  (set-face-attribute 'digit-groups-default-face nil
                      :foreground "light gray")
  )


(provide 'init-my-emacs-prettify)

;;; init-my-emacs-prettify.el ends here
