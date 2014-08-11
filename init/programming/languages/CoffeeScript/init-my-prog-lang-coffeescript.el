;;; init-my-prog-lang-coffeescript.el --- init for CoffeeScript
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ coffee-mode ]

;;; Usage:
;;
;;  Default Key Bindings
;;
;; Key 	Command
;; C-m, Return 	Insert newline and indent line
;; C-c C-<, backtab 	Indent line or region to left
;; C-c C-> 	Indent line or region to right
;; C-M-a 	Move to beginning of defun
;; C-M-e 	Move to end of defun
;; C-M-h 	Mark this defun
;; A-r, C-c C-k 	Compile buffer to JavaScript
;; A-R 	Compile content of region to JavaScript
;; A-M-r, C-c C-z 	Run CoffeeScript REPL
;; C-c C-l 	Send this line to REPL buffer
;; C-c C-r 	Send content of region to REPL buffer
;; C-c C-b 	Send content of buffer to REPL buffer
;; C-c C-o C-s 	Enable coffee-cos-mode
;;
;; Commands
;;
;; - coffee-repl :: launch a CoffeeScript REPL.
;; - coffee-compile-file :: compile buffer to JavaScript.
;; - coffee-compile-buffer :: Compile region to JavaScript
;; - coffee-watch :: Run coffee with the --watch flag on a directory or file.
;; - coffee-cos-mode :: Minor mode for compiling to JavaScript at save file.

(require 'coffee-mode)

;; (setq coffee-tab-width 4)
(setq coffee-args-compile '("-c")
      coffee-args-repl '("-i"))

(eval-after-load "coffee-mode"
  '(progn
     (define-key coffee-mode-map [f5] 'coffee-compile-buffer)
     (define-key coffee-mode-map (kbd "C-j") 'coffee-newline-and-indent)))

;;; Move to corresponding point in JavaScript file after compiling
;; You can archive this with `sourcemap' and following configuration.
;; (setq coffee-args-compile '("-c" "-m")) ;; generating sourcemap
;; (add-hook 'coffee-after-compile-hook 'sourcemap-goto-corresponding-point)

;; ;; If you want to remove sourcemap file after jumping corresponding point
;; (defun my/coffee-after-compile-hook (props)
;;   (sourcemap-goto-corresponding-point props)
;;   (delete-file (plist-get props :sourcemap)))
;; (add-hook 'coffee-after-compile-hook 'my/coffee-after-compile-hook)


(provide 'init-my-prog-lang-coffeescript)

;;; init-my-prog-lang-coffeescript.el ends here
