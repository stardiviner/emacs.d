;;; init-my-prog-code.el --- init Code writting for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:


;;; Code:

;;; [ prog-mode ]

;;; `prog-mode' is a major mode provided by Emacs. Typically, it is not used
;;; directly, instead many programming-related major modes are derived from this
;;; mode.

;; User can add things to `prog-mode-hook', which are executed for all
;; programming modes (that are derived from `prog-mode').
;; One benefit of using this mode is that global minor modes no longer have to
;; maintain a long list of suitable major modes. Instead, they can simply check
;; if a mode is derived from one of the base modes.
;; Other often used base modes include `special-mode' and `text-mode'.

;;; Usage:
;;
;; Some major programming modes is not included in `prog-mode' alist.
;; - check out what major modes which current in "prog-mode" list.
;; --> TODO:
;;
;; You can define a new major mode derived from ‘prog-mode’ using the following:
;;
;; (define-derived-mode alpha-mode prog-mode "Alpha"
;;   "Major mode for editing alpha files."
;;   ...)
;;
;; You can check if the major mode of the current buffer is derived from ‘prog-mode’ using:
;;
;; (derived-mode-p 'prog-mode)
;;
;; A global minor mode that will be enabled for all ‘prog-mode’ modes can be defined using:
;;
;; (define-global-minor-mode my-global-mode my-mode
;;   (lambda ()
;;     (when (derived-mode-p 'prog-mode)
;;       (my-mode 1))))

;;; TODO: add un-included programming modes into `prog-mode' alist variable.


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
