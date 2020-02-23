;;; init-SSH.el --- init for SSH. -*- lexical-binding: t; -*-

;;; Time-stamp: <2020-02-23 17:07:09 stardiviner>

;;; Commentary:



;;; Code:


;;; [ SSH ]

(use-package ssh-config-mode
  :ensure t
  :mode (("/\\.ssh/config\\'" . ssh-config-mode)
         ("/sshd?_config\\'" . ssh-config-mode)
         ("/known_hosts\\'" . ssh-known-hosts-mode)
         ("/authorized_keys\\'" . ssh-authorized-keys-mode)))

(use-package ssh-agency
  :ensure t)

(use-package ssh-tunnels
  :ensure t
  :commands (ssh-tunnels))

(use-package ssh-deploy
  :ensure t
  :after hydra
  :demand
  :hook ((after-save . ssh-deploy-after-save)
         (find-file . ssh-deploy-find-file))
  :config
  (ssh-deploy-line-mode)
  (ssh-deploy-hydra "C-c C-z"))



(provide 'init-SSH)

;;; init-SSH.el ends here
