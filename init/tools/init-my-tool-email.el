;;; init-my-tool-email.el --- init for Email
;;; -*- coding: utf-8 -*-

;;; Commentary:


;;; Code:


(unless (boundp 'email-prefix)
  (define-prefix-command 'email-prefix))
(define-key tools-prefix (kbd "m") 'email-prefix)


;;; [ mail-mode ] -- mail-mode is replaced with message-mode.


;;; [ message-mode ]

;; 'message-user-agent, 'mail-user-agent, 'gnus-user-agent, 'mu4e-user-agent,
(setq mail-user-agent 'message-user-agent
      compose-mail-user-agent-warnings nil
      ;; use `postfix' server instead of `smtpmail'.
      send-mail-function 'sendmail-send-it
      message-send-mail-function 'message-send-mail-with-sendmail
      ;; message-send-mail-real-function 'message-send-mail-with-sendmail
      )

(defun message-mode-setup ()
  ;; Org-mode structure
  (turn-on-orgstruct++)
  (turn-on-orgtbl)

  ;; add email name complete support
  (add-hook 'completion-at-point-functions
            'mail-completion-at-point-function nil t)
  (add-hook 'completion-at-point-functions
            'message-completion-function nil t)

  (footnote-mode 1)
  )

(add-hook 'message-mode-hook #'message-mode-setup)


;;; Email region

(defun email-region (start end)
  "Send region from `START' to `END' as the body of an email."
  (interactive "r")
  (let ((content (buffer-substring start end)))
    (compose-mail)
    (message-goto-body)
    (insert content)
    (message-goto-to)))

(define-key email-prefix (kbd "r") 'email-region)


;;; [ encrypt email ]

;;; - [C-c C-m C-e] :: (mml-secure-message-sign-encrypt)
;;;    This will add a tag at the beginning of the mail.
;;;    <#secure method=pgpmime mode=signencrypt>
;;;    the `mode=signencrypt' means:
;;;      - `sign'
;;;      - `encrypt'



;; (require 'init-gnus)
;; (require 'init-mu4e)



(defvar my-email-client 'mu4e
  "The value is 'gnus, 'mu4e, or t for default.")

(case my-email-client
  ;; Gnus
  ('gnus
   (define-key email-prefix (kbd "m") 'gnus)
   )

  ;; mu4e
  ('mu4e
   (define-key email-prefix (kbd "m") 'mu4e)
   ;; FIXME: let (setq mail-user-agent 'mu4e-user-agent)
   (if (eq 'mail-user-agent 'mu4e-user-agent)
       ;; there is upper set default mail-user-agent, so default [C-x m] will be change for mu4e
       (global-set-key (kbd "C-x m") 'mu4e-compose-new)
     )
   (define-key email-prefix (kbd "i") 'my-mu4e-jump-to-index)
   (define-key email-prefix (kbd "C") 'mu4e-compose-new)
   )

  ;; default
  (t
   (define-key email-prefix (kbd "m") 'compose-mail))
  )



(provide 'init-my-tool-email)

;;; init-my-tool-email.el ends here
