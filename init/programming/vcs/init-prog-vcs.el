;;; init-prog-vcs.el --- init Version Control System for Programming
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(unless (boundp 'prog-vcs-prefix)
  (define-prefix-command 'prog-vcs-prefix))
(global-set-key (kbd "C-c v") 'prog-vcs-prefix)

;;; [ bump-version ] -- Bump version (in batch) via Emacs.

(use-package bump-version
  :quelpa (bump-version :fetcher github :repo "atykhonov/emacs-bump-version")
  :commands (bump-version-patch bump-version-minor bump-version-major bump-version-release))

;;; [ vc ]

(use-package vc
  :custom (vc-handled-backends '(Git))
  :init (add-to-list 'display-buffer-alist '("\\*vc-diff\\*" . (display-buffer-below-selected))))

(require 'init-prog-vcs-git)

(require 'init-prog-vcs-git-github)
;; (require 'init-prog-vcs-git-gitlab)

(require 'init-prog-vcs-diff)
(require 'init-prog-vcs-changelog)
(require 'init-prog-vcs-commit)
(require 'init-prog-vcs-review)


(provide 'init-prog-vcs)

;;; init-prog-vcs.el ends here
