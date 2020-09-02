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

           (user-mail-address "numbchild@gmail.com")
           (user-full-name  "stardiviner")
           
           ;; It is important to set the mail-host-address otherwise emacs will
           ;; set the Message-Id header to something like “Message-ID:
           ;; 8737m2q8na.fsf@localhost.i-did-not-set--mail-host-address--so-tickle-me”.
           (mail-host-address user-mail-address)
           
           ;; send mail
           ;; send mail from localhost, NOTE: for Gmail will be in Junk Spam folder.
           (message-send-mail-function 'message-send-mail-with-sendmail)
           ;; send email with SMTP
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

;;; [ smtpmail ] -- send email to remote mail host

;;; [[info:smtpmail#Top][info:smtpmail#Top]]

;; (use-package smtpmail
;;   :custom (;; for debug
;;            ;; (smtpmail-debug-info t)
;;            ;; (smtpmail-debug-verb t)
;;            ;; specify sending mail agent
;;            (message-send-mail-function send-mail-function)
;;            (send-mail-function 'smtpmail-send-it)
;; 	         ;; configure Gmail SMTP server
;;            (smtpmail-smtp-server "smtp.gmail.com")
;;            (smtpmail-default-smtp-server smtpmail-smtp-server)
;; 	         ;; (smtpmail-stream-type 'ssl)
;;            (smtpmail-smtp-service 587)   ; "smtp": 25, "smtps": 587
;;            (smtpmail-smtp-user "numbchild@gmail.com")
;;            ;; (smtpmail-auth-credentials (expand-file-name (car auth-sources)))
;;            (smtpmail-local-domain "gmail.com")
;;            ;; queue sending email
;;            ;; (smtpmail-queue-mail t)
;;            ;; (smtpmail-queue-dir "~/Mails/queue/")
;;            ))
;;
;; (use-package sendmail
;;   :custom ((send-mail-function 'smtpmail-send-it)
;;            (mail-host-address user-mail-address)
;;            (mail-default-reply-to user-mail-address)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Retrieve Mail                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; procmail
(add-to-list 'auto-mode-alist '("\\.procmailrc\\'" . conf-mode))
;; getmail
(add-to-list 'auto-mode-alist '("\\.getmailrc\\'" . conf-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reading Mail [ Mail User Agent ] (MTA)                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'init-mu4e)



(provide 'init-tool-email)

;;; init-tool-email.el ends here
