;;; init-my-prog-lang-css-scss.el --- init for SCSS
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ scss-mode ]

;; Features
;;
;; Compilation of current file on save. (Disable by changing `scss-compile-at-save' to nil)
;; Flymake support, enable with M-x `flymake-mode'
;; Indentation and highlighting (Derived from CSS-mode)
;; Syntax highlighting for variables and inline comments.

(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

;; (setq exec-path (cons (expand-file-name "~/.gem/ruby/1.8/bin") exec-path))
;; (setq scss-sass-command "sass")


(provide 'init-my-prog-lang-css-scss)

;;; init-my-prog-lang-css-scss.el ends here
