;;; init-prog-sidebar.el --- init Emacs sidebar for Programming.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;; [ dired-sidebar ] -- Sidebar for Emacs leveraging Dired.

(use-package dired-sidebar
  :ensure t
  :defer t
  :commands (dired-sidebar-toggle-sidebar)
  :bind ("<f8>" . dired-sidebar-toggle-sidebar)
  :config
  ;; (setq dired-sidebar-use-custom-font t)
  ;; (setq dired-sidebar-face '(:family "Monaco" :height 120))
  (setq dired-sidebar-delay-auto-revert-updates nil)
  )



(provide 'init-prog-sidebar)

;;; init-prog-sidebar.el ends here
