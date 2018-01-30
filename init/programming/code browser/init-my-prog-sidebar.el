;;; init-my-prog-sidebar.el --- init Emacs sidebar for Programming.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;; [ dired-sidebar ] -- Sidebar for Emacs leveraging Dired.

(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :bind ("<f8>" . dired-sidebar-toggle-sidebar)
  :config
  (use-package all-the-icons-dired
    :ensure t)
  (if (and (display-graphic-p) (featurep 'all-the-icons-dired))
      (progn
        (add-hook 'dired-sidebar-mode-hook #'all-the-icons-dired-mode)
        (setq dired-sidebar-theme 'icons))
    (setq dired-sidebar-theme 'nerd))
  ;; (setq dired-sidebar-use-custom-font t)
  ;; (setq dired-sidebar-face '(:family "Monaco" :height 120))

  (setq dired-sidebar-delay-auto-revert-updates nil)
  )



(provide 'init-my-prog-sidebar)

;;; init-my-prog-sidebar.el ends here
