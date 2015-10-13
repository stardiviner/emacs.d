;;; init-my-prog-vcs-github.el --- init for GitHub
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ helm-open-github ]

;;; Usage:
;;
;; - `helm-open-github-from-commit'
;;   Open commit page from Commit ID
;; - `helm-open-github-from-file'
;;   Open file page from File Name
;; - `helm-open-github-from-issues'
;;   Open issue page from Issue ID
;; - `helm-open-github-from-pull-requests'
;;   Open pull request page from Pull Request ID

(use-package helm-open-github
  :config
  (unless (boundp 'my-prog-vcs-github-map)
    (define-prefix-command 'my-prog-vcs-github-map))
  (define-key my-prog-vcs-map (kbd "h") 'my-prog-vcs-github-map)

  (define-key my-prog-vcs-github-map (kbd "c") 'helm-open-github-from-commit)
  (define-key my-prog-vcs-github-map (kbd "f") 'helm-open-github-from-file)
  (define-key my-prog-vcs-github-map (kbd "i") 'helm-open-github-from-issues)
  (define-key my-prog-vcs-github-map (kbd "p") 'helm-open-github-from-pull-requests)
  )


;;; [ magit-gh-pulls ] -- conveniently manipulate Github’s pull requests

(use-package magit-gh-pulls
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
  )


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


;;; [ magithub ] -- working with GitHub

;; (require 'magithub)



(provide 'init-my-prog-vcs-github)

;;; init-my-prog-vcs-github.el ends here
