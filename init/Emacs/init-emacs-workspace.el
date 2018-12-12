;;; init-emacs-workspace.el --- init for Emacs workspace.

;;; Commentary:



;;; Code:

;;; [ eyebrowse ] -- A simple-minded way of managing window configs in Emacs.

(use-package eyebrowse
  :ensure t
  :custom (eyebrowse-keymap-prefix (kbd "C-x w"))
  :init (eyebrowse-mode t)
  :config
  (setq eyebrowse-new-workspace t
        eyebrowse-close-window-config-prompt t
        eyebrowse-mode-line-style nil ; it has very often invoked by `posn-at-point' affect performance.
        )

  ;; Fix side-window issue.
  ;; also save side and slot windows configuration.
  (add-to-list 'window-persistent-parameters '(window-side . writable))
  (add-to-list 'window-persistent-parameters '(window-slot . writable))

  (advice-add 'eyebrowse-create-window-config :after
              (lambda () (interactive)
                (command-execute 'eyebrowse-rename-window-config))))


(provide 'init-emacs-workspace)

;;; init-emacs-workspace.el ends here
