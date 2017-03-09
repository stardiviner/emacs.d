;;; init-my-prog-project.el --- init Project settings for Emacs

;;; Commentary:


;;; Code:


(unless (boundp 'my-prog-project-map)
  (define-prefix-command 'my-prog-project-map))
(global-set-key (kbd "C-c p") 'my-prog-project-map)


;;; [ Projectile ] -- minor mode to assist project management and navigation.

(use-package projectile
  :ensure t
  :defer t
  :init
  ;; custom default projectile mode-line :lighter for my custom mode-line format.
  (setq projectile-mode-line " Projectile") ; fix TRAMP hang on sending password.
  ;; Toggle `projectile-mode'
  ;; (projectile-global-mode)
  ;; OR
  (dolist (hook
           '(prog-mode-hook
             ))
    (add-hook hook 'projectile-mode))
  
  (setq projectile-keymap-prefix (kbd "C-c p"))
  
  :config
  ;; Caching: nil, (* 10 60) [10 minutes],
  (setq projectile-enable-caching nil ; nil: disable caching to fix TRAMP hang
                                        ; on sending password
        ;; remote file exists cache expire to 10 minutes
        projectile-file-exists-remote-cache-expire '(* 30 60)
        )
  ;; using Projectile everywhere
  (setq projectile-require-project-root t)
  ;; Completion Options
  (setq projectile-completion-system 'ivy
        projectile-use-git-grep t ; use `vc-git-grep'
        )

  ;; test
  (setq projectile-create-missing-test-files t)

  (use-package helm-projectile
    :ensure t
    :config
    (add-to-list 'helm-mini-default-sources 'helm-source-projectile-projects)
    )
  (use-package counsel-projectile
    :ensure t)
  )

;;; [ find-file-in-project ]

(use-package find-file-in-project
  :ensure t
  :defer t
  ;; :bind ("M-t" . find-file-in-project) ; same with `counsel-git'
  )

;;; [ project-shells ] -- manage the shell buffers for each project.

(use-package project-shells
  :ensure t
  :init
  (setq project-shells-keymap-prefix "C-x !")
  :config
  ;; (project-shells-setup projectile-mode-map)  

  (if (featurep 'project-shells)
      (with-eval-after-load 'project-shells
        (define-key project-shells-map (kbd "!") 'my-smart-eshell)))

  (global-project-shells-mode 1)
  ;; (add-hook 'prog-mode-hook #'project-shells-mode)
  )


(provide 'init-my-prog-project)

;;; init-my-prog-project.el ends here
