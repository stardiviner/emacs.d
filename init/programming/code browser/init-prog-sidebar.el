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
  :init (setq dired-sidebar-no-delete-other-windows t))

;; (use-package ibuffer-sidebar
;;   :ensure t
;;   :ensure ibuffer-projectile
;;   :ensure dired-sidebar
;;   :defer t
;;   :commands (ibuffer-sidebar-toggle-sidebar)
;;   :init
;;   ;; be toggled together with dired-sidebar.
;;   (defun +sidebar-toggle ()
;;     "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
;;     (interactive)
;;     (dired-sidebar-toggle-sidebar)
;;     (ibuffer-sidebar-toggle-sidebar)
;;     (ibuffer-projectile-set-filter-groups))
;;   (global-set-key (kbd "<f8>") '+sidebar-toggle)
;;   :config
;;   (setq ibuffer-sidebar-use-custom-font nil)
;;   (setq ibuffer-sidebar-face `(:family "Helvetica" :height 140)))



(provide 'init-prog-sidebar)

;;; init-prog-sidebar.el ends here
