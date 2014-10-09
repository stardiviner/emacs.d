;;; init-my-rainbow.el --- 
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; rainbow-mode -- colorize color names in buffers

;; (require 'rainbow-mode)

;; (eval-after-load 'rainbow-mode
;;   '(diminish 'rainbow-mode))

;; (dolist (hook
;;          '(emacs-lisp-mode-hook
;;            css-mode-hook
;;            html-mode-hook))
;;   (add-hook hook (lambda () (rainbow-mode 1))))


;;; [ rainbow-delimiters ] -- rainbow color parenthesis

(when (require 'rainbow-delimiters nil 'noerror)
  (rainbow-delimiters-mode t)
  ;; (global-rainbow-delimiters-mode) ; global
  ;; enable in all programming-related modes
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  ;; enable in specific modes
  ;; (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  )


;;; rainbow-identifiers

;; (require 'rainbow-identifiers)

;; (add-hook 'prog-mode-hook 'rainbow-identifiers-mode)


;;; rainbow-block

;; (require 'rainbow-blocks)

;; (add-hook 'clojure-mode-hook 'rainbow-blocks-mode)


(provide 'init-my-rainbow)

;;; init-my-rainbow.el ends here
