;;; init-my-tool-irc.el --- init for IRC.

;;; Commentary:



;;; Code:


(unless (boundp 'irc-prefix)
  (define-prefix-command 'irc-prefix))
(define-key tools-prefix (kbd "i") 'irc-prefix)


(require 'init-erc)


(provide 'init-my-tool-irc)

;;; init-my-tool-irc.el ends here
