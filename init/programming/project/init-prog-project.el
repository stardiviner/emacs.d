;;; init-prog-project.el --- init Project settings for Emacs

;;; Commentary:


;;; Code:


(unless (boundp 'project-prefix)
  (define-prefix-command 'project-prefix))
(global-set-key (kbd "C-c p") 'project-prefix)


;;; [ projectile ] -- minor mode to assist project management and navigation.

(use-package projectile
  :ensure t
  :delight projectile-mode
  :commands (projectile-mode)
  :init (projectile-mode 1)
  (setq projectile-completion-system 'ivy
        projectile-use-git-grep t)
  ;; testing
  (setq projectile-create-missing-test-files t)
  (with-eval-after-load 'projectile
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)))

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
