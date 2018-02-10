;;; init-my-emacs-prettify.el --- init for prettify Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ pretty-symbols ]

(use-package pretty-symbols
  :ensure t
  :init
  (global-prettify-symbols-mode 1)
  :config
  (setq prettify-symbols-unprettify-at-point 'right-edge)

  (setq pretty-symbol-categories '(lambda relational logical))
  ;; TODO: add symbols like this:
  ;; (add-to-list 'prettify-symbols-alist '("lambda" . 955))

  (advice-add 'prettify-symbols-mode :before
              #'(lambda (&rest _r)
                  (cl-case major-mode
                    (haskell-mode
                     (push '("\\"       . ?λ) prettify-symbols-alist))
                    (js2-mode
                     (push '("function" . ?λ) prettify-symbols-alist))
                    (lua-mode
                     (push '("function" . ?λ) prettify-symbols-alist))
                    (verilog-mode
                     (push '("begin"    . ?\{) prettify-symbols-alist)
                     (push '("end" . ?}) prettify-symbols-alist)))))

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
  (add-hook 'org-mode-hook
            (lambda ()
              (if (not (equal (buffer-name) "Diary.org"))
                  (digit-groups-mode 1))))
  )


(provide 'init-my-emacs-prettify)

;;; init-my-emacs-prettify.el ends here
