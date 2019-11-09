;;; init-emacs-macro.el --- init for Emacs macro.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Macro ] -- [C-x C-k]

;; file to save macros:
(load-file (expand-file-name "init/macros/macros" user-emacs-directory))

(define-key kmacro-keymap (kbd "s") 'kmacro-start-macro)
(define-key kmacro-keymap (kbd "e") 'kmacro-end-macro)
(define-key kmacro-keymap (kbd "E") 'edit-kbd-macro)
(define-key kmacro-keymap (kbd "c") 'kmacro-end-or-call-macro)
(define-key kmacro-keymap (kbd "C") 'kmacro-call-macro)
(define-key kmacro-keymap (kbd "v") 'kmacro-view-macro)


(provide 'init-emacs-macro)

;;; init-emacs-macro.el ends here
