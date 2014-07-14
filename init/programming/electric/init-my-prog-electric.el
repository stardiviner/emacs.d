;;; init-my-prog-electric.el --- init electric stuff.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; Rainbow Mode (rainbow-mode.el -- colorize color names in buffers)

(require 'rainbow-mode)
(eval-after-load 'rainbow-mode
  '(diminish 'rainbow-mode))

(dolist (hook
         '(emacs-lisp-mode-hook
           css-mode-hook
           html-mode-hook))
  (add-hook hook (lambda () (rainbow-mode 1))))


;;; Rainbow Delimiters

(when (require 'rainbow-delimiters nil 'noerror)
  (rainbow-delimiters-mode t)
  ;; (global-rainbow-delimiters-mode) ; global
  ;; enable in all programming-related modes
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  ;; enable in specific modes
  ;; (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  )



;; (require 'init-my-paredit)
;; (require 'init-my-autopair)

(require 'init-my-smartparens)



(provide 'init-my-prog-electric)

;;; init-my-prog-electric.el ends here
