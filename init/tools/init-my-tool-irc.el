;;; init-my-tool-irc.el --- init for IRC.

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------

(unless (boundp 'my-irc-map)
  (define-prefix-command 'my-irc-map))
(define-key my-tools-prefix (kbd "i") 'my-irc-map)


(require 'init-erc)

;;; ----------------------------------------------------------------------------

(provide 'init-my-tool-irc)

;;; init-my-tool-irc.el ends here
