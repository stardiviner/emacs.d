;;; init-tool-social-network.el --- init for Social Networks

;;; Commentary:



;;; Code:
;;; [ mastodon ] -- A GNU Social-compatible microblogging server.

(use-package mastodon
  :ensure t
  :bind (:map tools-prefix ("S" . mastodon))
  :init
  (add-to-list 'display-buffer-alist
               '("^\\*new toot\\*" . (display-buffer-below-selected)))
  :commands (mastodon mastodon-toot)
  :config
  ;; - "https://social.bytesexual.net/"
  ;; - "https://mastodon.social"
  (setq mastodon-instance-url "https://mastodon.social"
        ;; mastodon-token-file (concat user-emacs-directory "mastodon.plstore")
        )
  )

(provide 'init-tool-social-network)

;;; init-tool-social-network.el ends here
