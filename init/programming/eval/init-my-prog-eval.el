;;; init-my-prog-eval.el --- init for interactive evaluation.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(unless (boundp 'my-prog-eval-map)
  (define-prefix-command 'my-prog-eval-map))
(global-set-key (kbd "C-c e") 'my-prog-eval-map)


;;; [ evalator ]

(use-package evalator
  :ensure t
  :config
  (define-key my-prog-eval-map (kbd "e") 'evalator)
  (define-key my-prog-eval-map (kbd "x") 'evalator-explicit)
  (define-key my-prog-eval-map (kbd "r") 'evalator-resume)
  (define-key my-prog-eval-map (kbd "i") 'evalator-insert-equiv-expr)

  ;; auto detect context
  (setq evalator-config-mode-context-alist nil)
  (add-to-list 'evalator-config-mode-context-alist
               '(ruby-mode . evalator-ruby-context))
  (add-to-list 'evalator-config-mode-context-alist
               '(clojure-mode . evalator-clojure-context))
  )

(provide 'init-my-prog-eval)

;;; init-my-prog-eval.el ends here
