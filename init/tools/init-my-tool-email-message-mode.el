;;; init-my-tool-email-message-mode.el --- init Email message mode
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(add-hook 'message-mode-hook
          (lambda ()
            (turn-on-orgstruct) ; Org-struct minor mode
            (turn-on-orgstruct++)
            ;; enable Orgtbl minor mode in message-mode.
            (turn-on-orgtbl)))


;;; [ boxquote ]

;; ,----[ Sample quote ]
;; | This is a sample quote.  Notice that it looks good even if you
;; | send it to someone who reads mail in a proportional font (probably
;;                                                             | a non-hacker...).
;; `----

;;; Usage:
;;
;; - `boxquote-region' :: boxquote region.

(require 'boxquote)

;; (setq boxquote-title-format "[ %s ]")

;; `message-completion-function' (like capf)
;; (setq message-expand-name-databases '(bbdb eudb))

(define-key narrow-map (kbd "q") 'boxquote-narrow-to-boxquote-content)

(unless (boundp 'my-boxquote-map)
  (define-prefix-command 'my-boxquote-map))
(define-key my-tools-prefix-map (kbd "q") 'my-boxquote-map)

(define-key my-boxquote-map (kbd "q") 'boxquote-boxquote)
(define-key my-boxquote-map (kbd "u") 'boxquote-unbox)
(define-key my-boxquote-map (kbd "t") 'boxquote-text)
(define-key my-boxquote-map (kbd "U") 'boxquote-unbox-region)
(define-key my-boxquote-map (kbd "r") 'boxquote-region)
(define-key my-boxquote-map (kbd "b") 'boxquote-buffer)
(define-key my-boxquote-map (kbd "f") 'boxquote-defun)
(define-key my-boxquote-map (kbd "c") 'boxquote-shell-command)
(define-key my-boxquote-map (kbd "F") 'boxquote-describe-function)
(define-key my-boxquote-map (kbd "K") 'boxquote-describe-key)
(define-key my-boxquote-map (kbd "V") 'boxquote-describe-variable)
(define-key my-boxquote-map (kbd "C-w") 'boxquote-kill)
(define-key my-boxquote-map (kbd "C-y") 'boxquote-yank)
(define-key my-boxquote-map (kbd "p") 'boxquote-paragraph)



(provide 'init-my-tool-email-message-mode)

;;; init-my-tool-email-message-mode.el ends here
