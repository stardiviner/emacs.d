;;; init-SSH.el --- init for SSH. -*- lexical-binding: t; -*-

;;; Commentary:



;;; Code:


;;; [ SSH ]

(use-package ssh-config-mode
  :ensure t
  :defer t
  :mode (("/\\.ssh/config\\'" . ssh-config-mode)
         ("/sshd?_config\\'" . ssh-config-mode)
         ("/known_hosts\\'" . ssh-known-hosts-mode)
         ("/authorized_keys\\'" . ssh-authorized-keys-mode)))

(use-package ssh-agency
  :ensure t
  :defer t)

(use-package ssh-tunnels
  :ensure t
  :defer t
  :commands (ssh-tunnels))



(provide 'init-SSH)

;;; init-SSH.el ends here
