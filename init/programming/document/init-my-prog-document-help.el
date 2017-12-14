;;; init-my-prog-document-help.el --- init for Emacs help

;;; Commentary:



;;; Code:

;;; [ helpful ] -- A better Emacs *help* buffer.

(use-package helpful
  :ensure t
  :config
  ;; replace Emacs default keybindings.
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  ;; convenient keybinding:
  (global-set-key (kbd "C-h C-.") #'helpful-at-point)
  )



(provide 'init-my-prog-document-help)

;;; init-my-prog-document-help.el ends here
