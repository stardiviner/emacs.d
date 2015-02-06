;;; init-my-emacs-mail.el --- init for Email
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(setq mail-user-agent 'message-user-agent ; message-user-agent, 'mail-user-agent, ...
      compose-mail-user-agent-warnings nil
      ;; mail-setup-hook '(bbdb-insinuate-mail)
      )


;;; [ encrypt email ]

;;; - [C-c C-m C-e] :: (mml-secure-message-sign-encrypt)
;;;    This will add a tag at the beginning of the mail.
;;;    <#secure method=pgpmime mode=signencrypt>
;;;    the `mode=signencrypt' means:
;;;      - `sign'
;;;      - `encrypt'



(provide 'init-my-emacs-mail)

;;; init-my-emacs-mail.el ends here
