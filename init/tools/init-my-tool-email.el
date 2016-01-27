;;; init-my-tool-email.el --- init for Email
;;; -*- coding: utf-8 -*-

;;; Commentary:


;;; Code:


(unless (boundp 'my-email-prefix)
  (define-prefix-command 'my-email-prefix))
(define-key my-tools-prefix (kbd "m") 'my-email-prefix)


;;; [ mail-mode ] -- mail-mode is replaced with message-mode.


;;; [ message-mode ]

;; 'message-user-agent, 'mail-user-agent, 'gnus-user-agent, 'mu4e-user-agent,
(setq mail-user-agent 'message-user-agent
      compose-mail-user-agent-warnings nil
      )

(add-hook 'message-mode-hook
          (lambda ()
            ;; org-mode structure
            (turn-on-orgstruct)
            (turn-on-orgstruct++)
            (turn-on-orgtbl)

            ;; add email name complete support
            (setq-local completion-at-point-functions
                        '(mail-completion-at-point-function
                          message-completion-function))
            ))


;;; Email region

(defun email-region (start end)
  "Send region as the body of an email."
  (interactive "r")
  (let ((content (buffer-substring start end)))
    (compose-mail)
    (message-goto-body)
    (insert content)
    (message-goto-to)))

(define-key my-email-prefix (kbd "r") 'email-region)


;;; [ boxquote ]

;; ,----[ Sample quote ]
;; | This is a sample quote.  Notice that it looks good even if you
;; | send it to someone who reads mail in a proportional font (probably
;;                                                             | a non-hacker...).
;; `----

;;; Usage:
;;
;; - `boxquote-region' :: boxquote region.

(use-package boxquote
  :ensure t
  :config
  ;; (setq boxquote-title-format "[ %s ]")

  ;; `message-completion-function' (like capf)
  ;; (setq message-expand-name-databases '(bbdb eudb))

  (define-key narrow-map (kbd "q") 'boxquote-narrow-to-boxquote-content)

  (unless (boundp 'my-boxquote-map)
    (define-prefix-command 'my-boxquote-map))
  (define-key my-prog-comment-map (kbd "q") 'my-boxquote-map)

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
  )


;;; [ encrypt email ]

;;; - [C-c C-m C-e] :: (mml-secure-message-sign-encrypt)
;;;    This will add a tag at the beginning of the mail.
;;;    <#secure method=pgpmime mode=signencrypt>
;;;    the `mode=signencrypt' means:
;;;      - `sign'
;;;      - `encrypt'



(require 'init-my-tool-email-gnus)
(require 'init-my-tool-email-mu4e)


(provide 'init-my-tool-email)

;;; init-my-tool-email.el ends here
