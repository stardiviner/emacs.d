;;; init-my-prog-code.el --- init Code writting for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:


;;; Code:

;;; [ subword-mode ] -- editing code WithCamelCaseWritingLikeThis

;;; Usage:
;; - [C-h f subword-mode] :: get help. and keybindings.
;; - [M-f/b] :: forward/backward a word.
;; - [C-Right/Left] :: right/left a word.
;; - [M-@] :: mark a word.

(dolist (hook
         '(prog-mode-hook
           ruby-mode-hook)
         )
  (add-hook hook (lambda ()
                   (subword-mode +1))))


;;; [ glasses-mode ] -- make CamelCase identifiers easy look.

;;; insert a virtual underscore separator between the conjoined words, so
;;; `fooBarBaz' will look like `foo_Bar_Baz'.
;;;
;;; When glasses-mode is enabled, you should continue following the CamelCase
;;; style as Emacs will automagically insert the virtual separator, as needed,
;;; when you type a capitalized character.

;; (require 'glasses-mode)


(provide 'init-my-prog-code)

;;; init-my-prog-code.el ends here
