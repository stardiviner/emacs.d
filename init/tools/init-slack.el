;;; init-slack.el --- init for Slack

;;; Commentary:



;;; Code:

;;; [ slack ] -- Emacs interface for Slack.

(use-package slack
  :ensure t
  :config
  (setq slack-buffer-emojify nil)
  (setq slack-prefer-current-team t)

  )



(provide 'init-slack)

;;; init-slack.el ends here
