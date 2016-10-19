;;; init-my-prog-vcs-review.el --- init for Code Review
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
;;; [ magit-gerrit ] -- Magit plugin for Gerrit Code Review

(use-package magit-gerrit
  :ensure t
  :defer t
  :init
  ;; if remote url is not using the default gerrit port and
  ;; ssh scheme, need to manually set this variable
  ;; (setq-default magit-gerrit-ssh-creds "myid@gerrithost.org")

  ;; if necessary, use an alternative remote instead of 'origin'
  ;; (setq-default magit-gerrit-remote "gerrit")
  )

;;; ----------------------------------------------------------------------------

(provide 'init-my-prog-vcs-review)

;;; init-my-prog-vcs-review.el ends here
