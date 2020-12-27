;;; init-linux.el --- init for Linux
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ conf-mode ] -- Simple major mode for editing conf/ini/properties files.

;;; [ helm-system-packages ] -- Helm UI wrapper for system package managers.

(use-package helm-system-packages
  :ensure t
  :defer t
  :commands (helm-system-packages))

;;; [ systemd-mode ] -- Emacs major mode for editing systemd units.

(use-package systemd
  :ensure t
  :defer t)

;;; [ journalctl-mode ] -- Major mode to view journalctl's output in Emacs.

(use-package journalctl-mode
  :ensure t
  :defer t
  :commands (journalctl journalctl-boot journalctl-unit journalctl-user-unit))

;;; [ daemons ] -- An Emacs UI for managing init system services like Systemd.

(use-package daemons
  :ensure t
  :defer t
  :commands (daemons)
  :init (add-to-list 'display-buffer-alist '("^\\*daemons.*\\*" . (display-buffer-below-selected))))

;;; [ helm-systemd ] -- Helm's systemd interface.

(use-package helm-systemd
  :ensure t
  :defer t
  :commands (helm-systemd))

;;; [ ini-mode ] -- Major mode for Windows-style .ini files.

(use-package ini-mode
  :ensure t
  :mode "\\.ini\\'")

;;; [ nginx-mode ]

(use-package nginx-mode
  :ensure t
  :defer t
  :init
  (use-package company-nginx
    :ensure t
    :after nginx-mode
    :init (add-hook 'nginx-mode-hook #'company-nginx-keywords)))

;;; [ Conky ]

(use-package lua-mode
  :ensure t
  :defer t
  :mode (("\\.conkyrc\\'" . lua-mode)))

;;; [ PulseAudio ]

(add-to-list 'auto-mode-alist '("\\.pa\\'" . conf-mode))

;;; [ SystemTap ]

;; (use-package systemtap-mode
;;   :ensure t
;;   :defer t)

;;; Arch PKGBUILD (pkgbuild-mode)
(use-package pkgbuild-mode
  :ensure t
  :defer t
  :mode ("/PKGBUILD\\'" . pkgbuild-mode))

;;; [ pacfiles-mode ] -- pacnew and pacsave merging tool.

;; (use-package pacfiles-mode
;;   :ensure t)

;;; [ arch-packer ] -- Arch Linux package management frontend for pacman and pacaur.

;; (use-package arch-packer
;;   :ensure t
;;   :defer t
;;   :config
;;   (setq arch-packer-default-command "pacaur")
;;   )

;;========================== [ nftables config files ] =========================

(add-to-list 'auto-mode-alist '("\\.nft\\'" . conf-mode))

;;; [ bluetooth ] -- A Major mode for Bluetooth devices.

(use-package bluetooth
  :ensure t
  :defer t
  :commands (bluetooth-list-devices))


(provide 'init-linux)

;;; init-linux.el ends here
