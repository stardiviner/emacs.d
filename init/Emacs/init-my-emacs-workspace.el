;;; init-my-emacs-workspace.el --- init for Emacs workspace.

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
;;; [ perspeen ] -- A emacs plugin for multi workspace.

;; (use-package perspeen
;;   :ensure t
;;   :config
;;   ;; (setq perspeen-use-tab t)
;;   (setq perspeen-keymap-prefix (kbd "C-z"))
;;   (define-key perspeen-command-map (kbd "k") 'perspeen-delete-ws)
;;   (define-key perspeen-command-map (kbd "r") 'perspeen-rename-ws)
;;   (define-key perspeen-command-map (kbd "s") 'perspeen-goto-ws)
;;   (perspeen-mode 1)

;;   ;; (use-package helm-perspeen
;;   ;;   :ensure t
;;   ;;   :config
;;   ;;   (with-eval-after-load 'helm-buffers
;;   ;;     (add-to-list 'helm-mini-default-sources 'helm-source-perspeen-tabs)
;;   ;;     (add-to-list 'helm-mini-default-sources 'helm-source-perspeen-workspaces))
;;   ;;   )
;;   )


;;; ----------------------------------------------------------------------------

(provide 'init-my-emacs-workspace)

;;; init-my-emacs-workspace.el ends here
