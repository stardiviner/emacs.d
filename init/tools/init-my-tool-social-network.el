;;; init-my-tool-social-network.el --- init for Social Networks

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
;;; [ mastodon ] -- A GNU Social-compatible microblogging server.

(use-package mastodon
  :ensure t
  :config
  (setq mastodon-instance-url "https://social.bytesexual.net/" ; "https://mastodon.social"
        ;; mastodon-token-file (concat user-emacs-directory "mastodon.plstore")
        )
  )


;;; ----------------------------------------------------------------------------

(provide 'init-my-tool-social-network)

;;; init-my-tool-social-network.el ends here
