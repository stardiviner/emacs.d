;;; init-my-emacs-macro.el --- init for Emacs macro.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Macro ]

;; file to save macros:
(load-file "~/.emacs.d/init/macros/macros")

(define-key kmacro-keymap (kbd "s") 'kmacro-start-macro)
(define-key kmacro-keymap (kbd "e") 'kmacro-end-macro)
(define-key kmacro-keymap (kbd "E") 'edit-kbd-macro)
(define-key kmacro-keymap (kbd "c") 'kmacro-end-or-call-macro)
(define-key kmacro-keymap (kbd "C") 'kmacro-call-macro)
(define-key kmacro-keymap (kbd "v") 'kmacro-view-macro)


(provide 'init-my-emacs-macro)

;;; init-my-emacs-macro.el ends here
