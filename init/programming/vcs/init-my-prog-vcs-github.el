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

(require 'helm-open-github)

;; (setq helm-open-github-from-commit 100)

(unless (boundp 'my-prog-vcs-github-map)
  (define-prefix-command 'my-prog-vcs-github-map))
(define-key my-prog-vcs-map (kbd "h") 'my-prog-vcs-github-map)

(define-key my-prog-vcs-github-map (kbd "c") 'helm-open-github-from-commit)
(define-key my-prog-vcs-github-map (kbd "f") 'helm-open-github-from-file)
(define-key my-prog-vcs-github-map (kbd "i") 'helm-open-github-from-issues)
(define-key my-prog-vcs-github-map (kbd "p") 'helm-open-github-from-pull-requests)


(provide 'init-my-prog-vcs-github)

;;; init-my-prog-vcs-github.el ends here
