;;; init-emacs-workspace.el --- init for Emacs workspace.

;;; Commentary:



;;; Code:

;;; [ eyebrowse ] -- A simple-minded way of managing window configs in Emacs.

(use-package eyebrowse
  :ensure t
  :defer t
  :custom (eyebrowse-keymap-prefix (kbd "C-x w"))
  :hook (after-init . eyebrowse-mode)
  :custom ((eyebrowse-new-workspace t)
           (eyebrowse-close-window-config-prompt t)
           ;; it has very often invoked by `posn-at-point' affect performance.
           (eyebrowse-mode-line-style 'current))
  :config
  ;; Fix side-window issue.
  ;; also save side and slot windows configuration.
  (add-to-list 'window-persistent-parameters '(window-side . writable))
  (add-to-list 'window-persistent-parameters '(window-slot . writable))

  (advice-add 'eyebrowse-create-window-config :after
              (lambda () (interactive)
                (command-execute 'eyebrowse-rename-window-config))))

;;; [ persp-mode ] -- windows/buffers sets shared among frames + save/load.

;; (use-package persp-mode
;;   :ensure t
;;   :init (setq persp-keymap-prefix (kbd "C-x p"))
;;   :hook (after-init . persp-mode))

;;; [ spacebar ] -- Workspaces Tabbar.

;; (use-package spacebar
;;   :ensure t
;;   ;; :custom (spacebar-keymap-prefix (kbd "C-c w"))
;;   ;; :bind-keymap ("C-c w" . spacebar-command-map)
;;   :config (spacebar-mode))

;;; [ tab-bar ] -- frame-local tabs with named persistent window configurations.

;; (use-package tab-bar
;;   :commands (tab-bar-mode)
;;   :custom ((tab-bar-select-tab-modifiers '(control))
;;            (tab-bar-tab-hints t)))

;;; [ tab-line ] -- window-local tabs with window buffers.

;; (use-package tab-line
;;   :commands (global-tab-line-mode tab-line-mode))


(provide 'init-emacs-workspace)

;;; init-emacs-workspace.el ends here
