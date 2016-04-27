;;; init-linux.el --- init for Linux
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ systemd-mode ] -- Emacs major mode for editing systemd units.

(use-package systemd
  :ensure t
  :config
  (setq systemd-use-company-p t)
  )


;;; [ helm-systemd ] -- helm interface to control systemd units.

(use-package helm-systemd
  :ensure t)


;;; [ nginx-mode ]

(use-package nginx-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist
               '("nginx\\.conf\\'" . nginx-mode)
               '("/etc/nginx/.*" . nginx-mode))
  )


;;; [ apache-mode ]

(use-package apache-mode
  :ensure t)


(provide 'init-linux)

;;; init-linux.el ends here
