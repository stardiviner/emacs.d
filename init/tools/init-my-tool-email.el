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


;;; [ encrypt email ]

;;; - [C-c C-m C-e] :: (mml-secure-message-sign-encrypt)
;;;    This will add a tag at the beginning of the mail.
;;;    <#secure method=pgpmime mode=signencrypt>
;;;    the `mode=signencrypt' means:
;;;      - `sign'
;;;      - `encrypt'



(require 'init-gnus)
(require 'init-mu4e)


(provide 'init-my-tool-email)

;;; init-my-tool-email.el ends here
