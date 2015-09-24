;;; init-my-tool-email.el --- init for Email
;;; -*- coding: utf-8 -*-

;;; Commentary:


;;; Code:

;;; [ mail-mode ] -- mail-mode is replaced with message-mode.


;;; [ message-mode ]

;; message-user-agent, 'mail-user-agent, ...
(setq mail-user-agent 'message-user-agent
      compose-mail-user-agent-warnings nil
      )


;;; [ encrypt email ]

;;; - [C-c C-m C-e] :: (mml-secure-message-sign-encrypt)
;;;    This will add a tag at the beginning of the mail.
;;;    <#secure method=pgpmime mode=signencrypt>
;;;    the `mode=signencrypt' means:
;;;      - `sign'
;;;      - `encrypt'



(require 'init-my-tool-email-message-mode)
;; (require 'init-my-tool-email-gnus)
(require 'init-my-tool-email-mu4e)



(provide 'init-my-tool-email)

;;; init-my-tool-email.el ends here
