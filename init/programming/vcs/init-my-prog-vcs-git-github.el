;;; init-my-prog-vcs-git-github.el --- init for GitHub
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ magithub ] -- Magit interfaces for GitHub.

(use-package magithub
  :ensure t)


;;; [ magit-gerrit ] -- Magit plugin for Gerrit Code Review

;;; Usage:
;;
;;; Workflow
;;
;; - Check out branch, make changes, and commit...
;; - Gerrit Push Commit for Code Review => T P
;; - Gerrit Add Reviewer => T A (optional)
;; - Wait for code review and verification (approvals updated in magit-status)
;; - Gerrit Submit Review => T S

;; (require 'magit-gerrit)

;;; For simple setups, it should be enough to set the default value for
;;; `magit-gerrit-ssh-creds' and `magit-gerrit-remote'.
;;
;;; if remote url is not using the default gerrit port and
;;; ssh scheme, need to manually set this variable
;; (setq-default magit-gerrit-ssh-creds "myid@gerrithost.org")
;;
;;; if necessary, use an alternative remote instead of 'origin'
;; (setq-default magit-gerrit-remote "gerrit")

;;; For per project configurations, consider using buffer local or directory local variables.
;;
;; /home/dev/code/prj1/.dir-locals.el:
;; ((magit-mode .
;;              ((magit-gerrit-ssh-creds . "dev_a@prj1.server.com")
;;               (magit-gerrit-remote . "gerrit"))))


(provide 'init-my-prog-vcs-git-github)

;;; init-my-prog-vcs-git-github.el ends here
