;;; init-jetbrains.el --- Emacs-JetBrains IDE bridge.

;;; Commentary:



;;; Code:
;;; [ jetbrains ] -- Emacs-JetBrains IDE bridge.

(use-package jetbrains
  :ensure t
  :commands (jetbrains-create-dir-local-file
             jetbrains-open-project
             jetbrains-open-buffer-file))



(provide 'init-jetbrains)

;;; init-jetbrains.el ends here
