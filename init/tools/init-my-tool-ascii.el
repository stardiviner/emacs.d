;;; init-my-tool-ascii.el --- init for ASCII
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'ascii-prefix)
    (define-prefix-command 'ascii-prefix))
(define-key editing-prefix (kbd "a") 'ascii-prefix)

;;; [ ascii (ascii-mode) ] --

;; (require 'ascii)


(defun my-figlet-region (&optional b e)
  "Region select text, then execute command [M-x my-figlet-region]."
  (interactive "r")
  (shell-command-on-region b e "toilet" (current-buffer) t))

;;; [ helm-rage ] -- A Helm source for raging. Allows you to spice up your commit message with rage comics or various memes.

(use-package helm-rage
  :ensure t
  :commands (helm-rage)
  :bind (:map ascii-prefix
	      ("a" . helm-rage))
  )

;;; [ boxquote ] -- quote text with a semi-box.

(use-package boxquote
  :ensure t
  :init
  (unless (boundp 'prog-comment-prefix)
    (define-prefix-command 'prog-comment-prefix))
  (unless (boundp 'boxquote-prefix)
    (define-prefix-command 'boxquote-prefix))
  (define-key prog-comment-prefix (kbd "q") 'boxquote-prefix)
  :bind (:map narrow-map
              ("q" . boxquote-narrow-to-boxquote-content)
              :map boxquote-prefix
              ("q" . boxquote-boxquote)
              ("u" . boxquote-unbox)
              ("t" . boxquote-text)
              ("U" . boxquote-unbox-region)
              ("r" . boxquote-region)
              ("b" . boxquote-buffer)
              ("f" . boxquote-defun)
              ("c" . boxquote-shell-command)
              ("F" . boxquote-describe-function)
              ("K" . boxquote-describe-key)
              ("V" . boxquote-describe-variable)
              ("C-w" . boxquote-kill)
              ("C-y" . boxquote-yank)
              ("p" . boxquote-paragraph)
              )
  :config
  ;; (setq boxquote-title-format "[ %s ]")

  ;; `message-completion-function' (like `capf')
  ;; (setq message-expand-name-databases '(bbdb eudb))
  )


(provide 'init-my-tool-ascii)

;;; init-my-tool-ascii.el ends here
