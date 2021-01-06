;;; init-tool-email.el --- init for Email
;;; -*- coding: utf-8 -*-

;;; Commentary:

;;; - [C-x m] :: `compose-mail'

;;; Code:


(unless (boundp 'email-prefix)
  (define-prefix-command 'email-prefix))
(define-key tools-prefix (kbd "m") 'email-prefix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Composing Mail [ Mail User Agent ] (MTA)                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package message
  :custom (;; user agent
           ;; 'message-user-agent, 'mail-user-agent, 'gnus-user-agent, 'mu4e-user-agent,
           (mail-user-agent 'message-user-agent)
           
           (user-mail-address "numbchild@gmail.com") ; "stardiviner@outlook.com"
           (user-full-name  "Christopher M. Miles")
           
           ;; It is important to set the mail-host-address otherwise emacs will
           ;; set the Message-Id header to something like “Message-ID:
           ;; 8737m2q8na.fsf@localhost.i-did-not-set--mail-host-address--so-tickle-me”.
           (mail-host-address user-mail-address)
           
           ;; send mail
           ;; send email with `sendmail'
           (message-send-mail-function 'message-send-mail-with-sendmail)
           (sendmail-program (or (executable-find "msmtp") (executable-find "sendmail")))
           (mail-specify-envelope-from t)
           (mail-envelope-from 'header)
           (message-sendmail-envelope-from 'header)
           
           ;; send email with `smtpmail'
           ;; (message-send-mail-function 'message-smtpmail-send-it)
           )
  :init
  (defun my/message-mode-setup ()
    ;; add email name complete support
    (add-hook 'completion-at-point-functions 'mail-completion-at-point-function nil t)
    (add-hook 'completion-at-point-functions 'message-completion-function nil t)
    (footnote-mode 1))
  (add-hook 'message-mode-hook #'my/message-mode-setup)
  :config
  ;; cite region in message-mode.
  (defun message-cite-region (beg end &optional levels)
    "Cite region in message-mode."
    (interactive "*r\np")
    (goto-char beg)
    (beginning-of-line)
    (let*((first-line (line-number-at-pos))
          (last-line  (line-number-at-pos end))
          (num-lines  (1+ (- last-line first-line))) )
      (dotimes (unused num-lines)
        (while (looking-at ">") (delete-char 1)) ; TODO: away hard-code
        (when  (looking-at " ") (delete-char 1))
        (when (> levels 0)
          (insert-char ?> levels)
          (insert-char ?\ ))            ; this repairs ugly >quotes as well
        (forward-line 1) )
      ;; clean up
      (goto-char (point-min))
      (forward-line (1- last-line))
      (end-of-line)
      (when (region-active-p) (keyboard-quit))))

  (define-key message-mode-map (kbd "M-;") 'message-cite-region))

;;; [ org-msg ] -- Org Mode to send and reply to Email in HTML.

;; (use-package org-msg
;;   :ensure t
;;   :defer t
;;   :commands (org-msg-mode)
;;   :init
;;   (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil"
;;         org-msg-startup "hidestars indent inlineimages"
;;         org-msg-greeting-fmt "\nHi *%s*,\n\n"
;;         org-msg-greeting-name-limit 3
;;         org-msg-signature "
;;
;; Regards,
;;
;; #+begin_signature
;; -- [ stardiviner ]
;;
;; /One Emacs to rule them all/
;;
;; Blog: https://stardiviner.github.io/
;; IRC(freenode): stardiviner, Matrix: stardiviner
;; GPG: F09F650D7D674819892591401B5DF1C95AE89AC3
;; #+end_signature")
;;   (org-msg-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sign/Encrypt Mail                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; [ mml-sec.el ] -- Encrypt Email.

;;; - [C-c C-m C-e] :: (mml-secure-message-sign-encrypt)
;;;    This will add a tag at the beginning of the mail.
;;;    <#secure method=pgpmime mode=signencrypt>
;;;    the `mode=signencrypt' means:
;;;      - `sign'
;;;      - `encrypt'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Send Mail [ Mail Sending Agent ] (MSA)                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package sendmail
  :custom ((send-mail-function 'smtpmail-send-it)
           (mail-host-address user-mail-address)
           (mail-default-reply-to user-mail-address)))

;;; [ smtpmail ] -- send email to remote mail host

;;; [[info:smtpmail#Top][info:smtpmail#Top]]

(use-package smtpmail
  :defer t
  :custom (;; for debug
           ;; (smtpmail-debug-info t)
           ;; (smtpmail-debug-verb t)
           ;; specify sending mail agent
           (message-send-mail-function send-mail-function)
           (send-mail-function 'smtpmail-send-it)
           
	       ;; ;; configure Gmail SMTP server
           ;; (smtpmail-smtp-server "smtp.gmail.com")
           ;; (smtpmail-default-smtp-server smtpmail-smtp-server)
	       ;; ;; (smtpmail-stream-type 'ssl)
           ;; (smtpmail-smtp-service 587)  ; "smtp": 25, "smtps": 587
           ;; (smtpmail-smtp-user "numbchild@gmail.com")
           ;; ;; (smtpmail-auth-credentials (expand-file-name (car auth-sources)))
           ;; (smtpmail-local-domain "gmail.com")
           ;; (user-mail-address "numbchild@gmail.com")
           ;; (mail-default-reply-to "numbchild@gmail.com")
           
           ;; configure Microsoft Outlook SMTP server
           ;; $ ping -c 5 smtp.office365.com
           (smtpmail-smtp-server "smtp.office365.com")
           (smtpmail-default-smtp-server smtpmail-smtp-server)
	       (smtpmail-stream-type 'starttls)
           (smtpmail-smtp-service 587)
           (smtpmail-smtp-user "stardiviner@outlook.com")
           ;; (smtpmail-auth-credentials (expand-file-name (car auth-sources)))
           ;; (smtpmail-local-domain "outlook.com")
           ;; ====================================================================
           ;; Use Microsoft Outlook SMTP Server, but still reply to my Gmail
           ;; account by setting "To:" header. This is because Google Groups
           ;; require member permission to send email. Otherwise deliver
           ;; rejected by Google Groups.
           (user-mail-address "numbchild@gmail.com")
           (mail-default-reply-to "numbchild@gmail.com")
           (mu4e-compose-reply-to-address "numbchild@gmail.com")
           (mail-specify-envelope-from t)
           (mail-envelope-from nil)
           
           ;; queue sending email
           ;; (smtpmail-queue-mail t)
           ;; (smtpmail-queue-dir "~/Mails/queue/")
           ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Retrieve Mail                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package conf-mode
  :mode (("\\procmailrc\\'" . conf-mode)
         ("\\getmailrc\\'" . conf-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reading Mail [ Mail User Agent ] (MTA)                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'init-mu4e)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mailing List Archives                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package hyperkitty
;;   :ensure t
;;   :defer t
;;   :commands (hyperkitty)
;;   :custom (hyperkitty-mlists '(("test@mailman3.org" . "https://lists.mailman3.org/archives"))))



(provide 'init-tool-email)

;;; init-tool-email.el ends here
