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
  :init
  (setq helm-systemd-list-not-loaded t
        helm-systemd-list-all nil))

;;; [ daemons ] -- An Emacs UI for managing init system services.

(use-package daemons
  :ensure t
  :defer t
  :commands (daemons)
  :config
  ;; (add-to-list 'display-buffer-alist
  ;;              '("\\*daemons\\*" . (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist
               '("\\*daemons-output\\*" . (display-buffer-below-selected)))
  )

;;; [ nginx-mode ]

(use-package nginx-mode
  :ensure t
  :init
  (use-package company-nginx
    :ensure t
    :config
    (eval-after-load 'nginx-mode
      '(add-hook 'nginx-mode-hook #'company-nginx-keywords))
    ))

;;; [ apache-mode ]

(use-package apache-mode
  :ensure t
  :defer t)

;;; [ Conky ]

(use-package lua-mode
  :ensure t
  :defer t
  :mode (("\\.conkyrc\\'" . lua-mode)))

;;; [ ini-mode ] -- a major mode for *.ini files.

(use-package ini-mode
  :ensure t
  :defer t)

;;; [ PulseAudio ]

(add-to-list 'auto-mode-alist '("\\.pa\\'" . conf-mode))

;;; [ SystemTap ]

;; (use-package systemtap-mode
;;   :ensure t
;;   :defer t)

;;; Arch PKGBUILD (pkgbuild-mode)
(use-package pkgbuild-mode
  :ensure t
  :mode ("/PKGBUILD\\'" . pkgbuild-mode)
  ;; :init
  ;; (autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
  ;; (setq auto-mode-alist (append '(("/PKGBUILD\\'" . pkgbuild-mode)) auto-mode-alist))
  )

;;; [ arch-packer ] -- Arch Linux package management frontend for pacman and pacaur.

;; (use-package arch-packer
;;   :ensure t
;;   :defer t
;;   :config
;;   (setq arch-packer-default-command "pacaur")
;;   )


(provide 'init-linux)

;;; init-linux.el ends here
