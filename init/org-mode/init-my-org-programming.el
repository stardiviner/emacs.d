;;; init-my-org-programming.el --- init Org-mode for Programming.

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
;;; [ org-projectile ] -- Manage org-mode TODOs for your projectile projects.

(use-package org-projectile
  :ensure t
  :bind (("C-c n p" . org-projectile:project-todo-completing-read))
  :config
  ;; global projects todo file
  (setq org-projectile:projects-file
        (concat org-directory "/Programming/Projects/"))
  
  ;; per-project todo files
  (org-projectile:per-repo)
  (setq org-projectile:per-repo-filename "TODO.org")

  ;; prompt mode
  (org-projectile:prompt)

  ;; add files to `org-agenda'.
  (setq org-agenda-files (append org-agenda-files (org-projectile:todo-files)))

  (add-to-list 'org-capture-templates (org-projectile:project-todo-entry "p"))
  )


;;; ----------------------------------------------------------------------------

(provide 'init-my-org-programming)

;;; init-my-org-programming.el ends here
