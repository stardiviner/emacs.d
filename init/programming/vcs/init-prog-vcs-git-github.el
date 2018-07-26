;;; init-prog-vcs-git-github.el --- init for GitHub
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ ghub ] -- minuscule client library for the Github API.

(use-package ghub
  :ensure t
  :defer t)

;;; [ magithub ] -- Magit interfaces for GitHub.

(use-package magithub
  :ensure t
  :after magit
  :bind (:map prog-vcs-prefix ("n" . magithub-dashboard))
  :config
  (magithub-feature-autoinject t)
  (add-to-list 'display-buffer-alist
               '("^\\*magithub-dash\\*" (display-buffer-same-window)))
  (add-to-list 'display-buffer-alist
               '("^\\*magithub:.*\\*" (display-buffer-same-window)))
  )

;;; [ github-browse-file ] -- View the file you're editing in Emacs on GitHub.

(use-package github-browse-file
  :ensure t
  :defer t
  :init
  (unless (boundp 'git-gutter-prefix)
    (define-prefix-command 'git-gutter-prefix))
  :bind (:map git-gutter-prefix
              ("f" . github-browse-file)
              ("F" . github-browse-file-blame)
              ("M-c" . github-browse-commit))
  :config
  (setq github-browse-file-show-line-at-point t))



(provide 'init-prog-vcs-git-github)

;;; init-prog-vcs-git-github.el ends here
