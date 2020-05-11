;;; init-prog-vcs-git-github.el --- init for GitHub
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ ghub ] -- minuscule client library for the Github API.

(use-package ghub
  :ensure t
  :defer t)

;;; [ ghub+ ] -- A thick GitHub API client built on ghub.

(use-package ghub+
  :ensure t
  :defer t
  :commands (ghubp-get-notifications))



(provide 'init-prog-vcs-git-github)

;;; init-prog-vcs-git-github.el ends here
