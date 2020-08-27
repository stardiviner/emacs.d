;;; init-prog-project.el --- init Project settings for Emacs

;;; Commentary:


;;; Code:


(unless (boundp 'project-prefix)
  (define-prefix-command 'project-prefix))
(global-set-key (kbd "C-c p") 'project-prefix)


;;; [ projectile ] -- minor mode to assist project management and navigation.

(use-package projectile
  :ensure t
  :defer t
  :delight projectile-mode
  :commands (projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom ((projectile-completion-system 'ivy)
           (projectile-switch-project-action #'projectile-commander)
           (projectile-use-git-grep t)
           (projectile-create-missing-test-files t))
  :init (projectile-global-mode 1)
  (add-to-list 'display-buffer-alist '("^\\*Projectile Commander Help\\*" . (display-buffer-below-selected))))

;;; [ projectile-variable ] -- store project local variables.

(use-package projectile-variable
  :ensure t
  :defer t
  :commands (projectile-variable-put projectile-variable-get projectile-variable-alist))

;;; [ project-shells ] -- manage the shell buffers for each project.

;; (use-package project-shells
;;   :ensure t
;;   :defer t
;;   :preface (setq project-shells-keymap-prefix "C-c p M-!")
;;   :init (global-project-shells-mode 1))


(provide 'init-prog-project)

;;; init-prog-project.el ends here
