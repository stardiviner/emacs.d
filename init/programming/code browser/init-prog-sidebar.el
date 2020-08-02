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
  :custom ((dired-sidebar-no-delete-other-windows t))
  :config
  ;; disable `zoom-mode' before toggle `dired-sidebar'.
  (when (featurep 'zoom)
    (defvar dired-sidebar--zoom-mode-status nil)
    (advice-add 'dired-sidebar-show-sidebar :before
                (lambda (&optional b)
                  (setq dired-sidebar--zoom-mode-status zoom-mode)
                  (zoom--off)))
    (advice-add 'dired-sidebar-hide-sidebar :after #'zoom--on)))

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

;;; [ treemacs ] -- A tree style file explorer package.

(use-package treemacs
  :ensure t
  :ensure treemacs-all-the-icons)



(provide 'init-prog-sidebar)

;;; init-prog-sidebar.el ends here
