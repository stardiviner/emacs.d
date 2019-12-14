;;; init-prog-vcs.el --- init Version Control System for Programming
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(unless (boundp 'prog-vcs-prefix)
  (define-prefix-command 'prog-vcs-prefix))
(global-set-key (kbd "C-c v") 'prog-vcs-prefix)


(require 'init-prog-vcs-git)

(require 'init-prog-vcs-git-github)
;; (require 'init-prog-vcs-git-gitlab)

(require 'init-prog-vcs-diff)
(require 'init-prog-vcs-commit)
(require 'init-prog-vcs-review)


(provide 'init-prog-vcs)

;;; init-prog-vcs.el ends here
