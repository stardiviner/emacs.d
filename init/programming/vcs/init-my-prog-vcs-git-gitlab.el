;;; init-my-prog-vcs-git-gitlab.el --- init for GitLab
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ gitlab ] -- Emacs client for GitLab

(use-package gitlab
  :ensure t)


;;; [ ivy-gitlab ] -- Ivy interface to GitLab

(use-package ivy-gitlab
  :ensure t)


;;; [ helm-gitlab ] -- Helm interface to GitLab

;; (use-package helm-gitlab
;;   :ensure t)


(provide 'init-my-prog-vcs-git-gitlab)

;;; init-my-prog-vcs-git-gitlab.el ends here
