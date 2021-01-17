;;; init-bat.el --- init for Windows Batch Script. -*- lexical-binding: t; -*-

;;; Commentary:



;;; Code:

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



(provide 'init-bat)

;;; init-bat.el ends here
