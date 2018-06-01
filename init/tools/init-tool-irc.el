;;; init-tool-irc.el --- init for IRC.

;;; Commentary:



;;; Code:


(unless (boundp 'irc-prefix)
  (define-prefix-command 'irc-prefix))
(define-key tools-prefix (kbd "i") 'irc-prefix)


(require 'init-erc)
(require 'init-rcirc)


(provide 'init-tool-irc)

;;; init-tool-irc.el ends here
