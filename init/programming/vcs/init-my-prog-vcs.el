;;; init-my-prog-vcs.el --- init Version Control System for Programming
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(unless (boundp 'prog-vcs-prefix)
  (define-prefix-command 'prog-vcs-prefix))
(global-set-key (kbd "C-c v") 'prog-vcs-prefix)


(require 'init-my-prog-vcs-git)

(require 'init-my-prog-vcs-git-gutter)
(require 'init-my-prog-vcs-git-github)
;; (require 'init-my-prog-vcs-git-gitlab)

(require 'init-my-prog-vcs-diff)
(require 'init-my-prog-vcs-commit)
(require 'init-my-prog-vcs-review)


(provide 'init-my-prog-vcs)

;;; init-my-prog-vcs.el ends here
