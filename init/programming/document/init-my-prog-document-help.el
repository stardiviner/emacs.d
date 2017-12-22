;;; init-my-prog-document-help.el --- init for Emacs help

;;; Commentary:



;;; Code:

(add-to-list 'display-buffer-alist
             '("^\\*Help\\*$" (display-buffer-below-selected)))

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
  
  (add-to-list 'display-buffer-alist
               '("^\\*helpful.*\\*$" (display-buffer-same-window)))
  )



(provide 'init-my-prog-document-help)

;;; init-my-prog-document-help.el ends here
