;;; init-my-emacs-workspace.el --- init for Emacs workspace.

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
;;; [ perspeen ] -- A emacs plugin for multi workspace.

(use-package perspeen
  :ensure t
  :config
  (setq perspeen-keymap-prefix (kbd "C-z"))
  (perspeen-mode 1)
  )


;;; ----------------------------------------------------------------------------

(provide 'init-my-emacs-workspace)

;;; init-my-emacs-workspace.el ends here
