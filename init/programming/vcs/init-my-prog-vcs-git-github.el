;;; init-my-prog-vcs-git-github.el --- init for GitHub
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ magithub ] -- Magit interfaces for GitHub.

(use-package magithub
  :ensure t
  :config
  (magithub-feature-autoinject t))


(provide 'init-my-prog-vcs-git-github)

;;; init-my-prog-vcs-git-github.el ends here
