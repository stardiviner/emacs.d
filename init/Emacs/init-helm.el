;;; init-helm.el --- simple configure Helm
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Helm ] -- Helm is an Emacs incremental and narrowing framework.

(use-package helm
  :ensure t
  :defer t
  :bind (
         ;; ([remap execute-extended-command] . helm-M-x)
         ;; ("M-x" . helm-M-x)
         ;; ([remap switch-to-buffer] . helm-mini)
         ;; ([remap yank-pop] . helm-show-kill-ring)
         ([remap org-goto] . helm-org-in-buffer-headings) ; [C-c C-j] completion for Org headings
         )
  :load (helm-config helm-misc)
  ;; :init (helm-mode 1) (helm-top-poll-mode 1)
  :config
  (setq helm-split-window-inside-p t)
  (setq helm-mini-default-sources
        '(helm-source-buffers-list
          helm-source-bookmarks
          helm-source-recentf
          helm-source-buffer-not-found
          ))
  (setq helm-input-idle-delay 0.1) ; fix Helm fast respond to input caused failed issue.
  (setq helm-org-headings-fontify t))


(provide 'init-helm)

;;; init-helm.el ends here
