;;; init-communication.el --- init for Communication Tools.

;;; Commentary:



;;; Code:

;;; [ Matrix ] -- An open standard for decentralized persistent communication.

(use-package matrix-client
  :quelpa (matrix-client :fetcher github :repo "jgkamat/matrix-client-el")
  :commands (matrix-client)
  :config (setq matrix-client-show-images t))

;;; [ slack ] -- Emacs interface for Slack.

(use-package slack
  :ensure t
  :defer t
  :config
  (setq slack-buffer-emojify nil)
  (setq slack-prefer-current-team t)

  )



(provide 'init-communication)

;;; init-communication.el ends here