;;; init-my-rainbow.el --- 
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ rainbow-mode ] -- colorize color names in buffers

;; (load "~/.emacs.d/init/extensions/rainbow-mode-0.10.el")

(require 'rainbow-mode)

(eval-after-load 'rainbow-mode
  '(diminish 'rainbow-mode))

(dolist (hook
         '(emacs-lisp-mode-hook
           css-mode-hook
           html-mode-hook))
  (add-hook hook (lambda () (rainbow-mode 1))))


;;; [ rainbow-delimiters ] -- rainbow color parenthesis

(when (require 'rainbow-delimiters nil 'noerror)
  (rainbow-delimiters-mode t)
  ;; (global-rainbow-delimiters-mode) ; global
  ;; enable in all programming-related modes
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  ;; enable in specific modes
  ;; (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  )


;; you have two styles:
;; 1. :box t
;; 2. :inverse-video t
;; 3. :weight 'bold
(set-face-attribute 'rainbow-delimiters-depth-1-face nil
                    :foreground "#2aa198"
                    :weight 'bold)
(set-face-attribute 'rainbow-delimiters-depth-2-face nil
                    :foreground "#b58900"
                    :weight 'bold)
(set-face-attribute 'rainbow-delimiters-depth-3-face nil
                    :foreground "#268bd2"
                    :weight 'bold)
(set-face-attribute 'rainbow-delimiters-depth-4-face nil
                    :foreground "#dc322f"
                    :weight 'bold)
(set-face-attribute 'rainbow-delimiters-depth-5-face nil
                    :foreground "#859900"
                    :weight 'bold)
(set-face-attribute 'rainbow-delimiters-depth-6-face nil
                    :foreground "#268bd2"
                    :weight 'bold)
(set-face-attribute 'rainbow-delimiters-depth-7-face nil
                    :foreground "#cb4b16"
                    :weight 'bold)
(set-face-attribute 'rainbow-delimiters-depth-8-face nil
                    :foreground "#d33682"
                    :weight 'bold)
(set-face-attribute 'rainbow-delimiters-depth-9-face nil
                    :foreground "#839496"
                    :weight 'bold)

(set-face-attribute 'rainbow-delimiters-mismatched-face nil
                    :inherit 'show-paren-mismatch-face
                    )


;;; rainbow-identifiers

;; (require 'rainbow-identifiers)

;; (add-hook 'prog-mode-hook 'rainbow-identifiers-mode)


;;; rainbow-block

;; (require 'rainbow-blocks)

;; (add-hook 'clojure-mode-hook 'rainbow-blocks-mode)


(provide 'init-my-rainbow)

;;; init-my-rainbow.el ends here
