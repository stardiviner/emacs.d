;;; init-org-programming.el --- init Org-mode for Programming.

;;; Commentary:



;;; Code:

;;; [ org-projectile ] -- Manage org-mode TODOs for your projectile projects.

;; TODO: setup this correctly, check out org file.
;; TODO: add helper functions
(use-package org-projectile
  :ensure t
  :bind (:map Org-prefix ("C-p" . org-projectile-project-todo-completing-read))
  :config
  ;; global projects todo file
  (setq org-projectile-projects-file
        (concat org-directory "/Programming Code/Projects/"))
  ;; add files to `org-agenda'.
  (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
  ;; add org-mode capture templates.
  (add-to-list 'org-capture-templates (org-projectile-project-todo-entry))

  (use-package org-projectile-helm
    :ensure t
    :commands (org-projectile-helm-template-or-project))
  )


(provide 'init-org-programming)

;;; init-org-programming.el ends here
