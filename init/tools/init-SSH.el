;;; init-SSH.el --- init for SSH. -*- lexical-binding: t; -*-

;;; Time-stamp: <2020-05-11 22:40:35 stardiviner>

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

(use-package ssh-deploy
  :ensure t
  :after hydra
  :defer t
  :commands (ssh-deploy-after-save ssh-deploy-find-file)
  :hook ((after-save . ssh-deploy-after-save)
         (find-file . ssh-deploy-find-file))
  :config (ssh-deploy-line-mode)
  (ssh-deploy-hydra "C-c C-z"))



(provide 'init-SSH)

;;; init-SSH.el ends here
