;;; init-my-emacs-edit-rectangle.el --- init for rectangle selection.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------

(unless (boundp 'rectangle-map)
  (define-prefix-command 'rectangle-map))
(global-set-key (kbd "C-x e r") 'rectangle-map)

(global-unset-key (kbd "C-x r N"))
(global-unset-key (kbd "C-x r t"))
(global-unset-key (kbd "C-x r c"))
(global-unset-key (kbd "C-x r i"))
(global-unset-key (kbd "C-x r n"))
(global-unset-key (kbd "C-x r o"))
(global-unset-key (kbd "C-x r y"))
(global-unset-key (kbd "C-x r k"))
(global-unset-key (kbd "C-x r d"))
(global-unset-key (kbd "C-x r M-w"))

(define-key rectangle-map (kbd "r") 'rectangle-mark-mode)
(define-key rectangle-map (kbd "m") 'set-rectangular-region-anchor)
(define-key rectangle-map (kbd "c") 'copy-rectangle-to-register)
(define-key rectangle-map (kbd "M-w") 'copy-rectangle-as-kill)
(define-key rectangle-map (kbd "y") 'yank-rectangle)
(define-key rectangle-map (kbd "x") 'clear-rectangle)
(define-key rectangle-map (kbd "d") 'delete-rectangle)
(define-key rectangle-map (kbd "k") 'kill-rectangle)
(define-key rectangle-map (kbd "o") 'open-rectangle)
(define-key rectangle-map (kbd "t") 'string-rectangle)
(define-key rectangle-map (kbd "N") 'rectangle-number-lines)


;;; ----------------------------------------------------------------------------

(provide 'init-my-emacs-edit-rectangle)

;;; init-my-emacs-edit-rectangle.el ends here
