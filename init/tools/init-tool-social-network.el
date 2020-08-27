;;; init-tool-social-network.el --- init for Social Networks

;;; Commentary:



;;; Code:
;;; [ mastodon ] -- A GNU Social-compatible microblogging server.

(use-package mastodon
  :ensure t
  ;; - "https://social.bytesexual.net/"
  ;; - "https://mastodon.social"
  :custom ((mastodon-instance-url "https://mastodon.social")
           (mastodon-token-file (concat user-emacs-directory "mastodon.plstore")))
  :bind (:map tools-prefix ("S" . mastodon))
  :commands (mastodon mastodon-toot)
  :init (add-to-list 'display-buffer-alist '("^\\*new toot\\*" . (display-buffer-below-selected))))

(provide 'init-tool-social-network)

;;; init-tool-social-network.el ends here
