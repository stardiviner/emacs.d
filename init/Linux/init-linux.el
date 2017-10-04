;;; init-linux.el --- init for Linux
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ systemd-mode ] -- Emacs major mode for editing systemd units.

(use-package systemd
  :ensure t
  :defer t
  :config
  (add-hook 'systemd-mode-hook
            (lambda ()
              (my-company-add-backend-locally 'systemd-company-backend)) :local t)
  )


;;; [ helm-systemd ] -- helm interface to control systemd units.

(use-package helm-systemd
  :ensure t
  :defer t
  :config
  (setq helm-systemd-list-not-loaded t
        helm-systemd-list-all nil
        )
  )


;;; [ nginx-mode ]

(use-package nginx-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist
               '("nginx\\.conf\\'" . nginx-mode)
               '("/etc/nginx/.*" . nginx-mode))
  )


;;; [ apache-mode ]

(use-package apache-mode
  :ensure t
  :defer t)

;;; [ ini-mode ] -- a major mode for *.ini files.

(use-package ini-mode
  :ensure t)

;;; [ PulseAudio ]

(add-to-list 'auto-mode-alist '("\\.pa\\'" . conf-mode))

;;; [ SystemTap ]

;; (use-package systemtap-mode
;;   :ensure t)

;;; [ arch-packer ] -- Arch Linux package management frontend for pacman and pacaur.

;; (use-package arch-packer
;;   :ensure t
;;   :config
;;   (setq arch-packer-default-command "pacaur")
;;   )


(provide 'init-linux)

;;; init-linux.el ends here
