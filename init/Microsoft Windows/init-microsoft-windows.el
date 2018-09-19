;;; init-microsoft-windows.el --- init for Microsoft Windows.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
;;; [ ini-mode ] -- Major mode for Windows-style .ini files.

(use-package ini-mode
  :ensure t
  :mode "\\.ini\\'")

;;; [ bat-mode ] -- Emacs built-in major mode for editing DOS/Windows scripts.

(require 'bat-mode)

;;; [ bmx-mode ] -- Batch Mode eXtras.

(use-package bmx-mode
  :ensure t
  :config
  ;; (bmx-mode-setup-defaults)
  (add-hook 'bat-mode-hook #'bmx-mode)
  (defun my/bmx-mode-company-setup ()
    (make-local-variable 'company-backends)
    (add-to-list 'company-backends #'bmx--company-label-backend)
    (add-to-list 'company-backends #'bmx--company-variable-backend)
    (add-hook 'company-completion-finished-hook #'bmx--company-completion-finished-hook))
  (add-hook 'bmx-mode-hook #'my/bmx-mode-company-setup))

;;; ----------------------------------------------------------------------------

(provide 'init-microsoft-windows)

;;; init-microsoft-windows.el ends here
