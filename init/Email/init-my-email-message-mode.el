;;; init-my-email-message-mode.el --- init Email message mode
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(add-hook 'message-mode-hook
          (lambda ()
            (turn-on-orgstruct) ; Org-struct minor mode
            (turn-on-orgstruct++)
            ;; enable Orgtbl minor mode in message-mode.
            (turn-on-orgtbl)))


(provide 'init-my-email-message-mode)

;;; init-my-email-message-mode.el ends here
