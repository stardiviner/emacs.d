;;; init-helm.el --- simple configure Helm
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Helm ] -- Helm is an Emacs incremental and narrowing framework.

(use-package helm
  :ensure t
  :defer t
  :commands (helm-mode)
  :bind (
         ;; ([remap execute-extended-command] . helm-M-x)
         ;; ("M-x" . helm-M-x)
         ;; ([remap switch-to-buffer] . helm-mini)
         ;; ([remap yank-pop] . helm-show-kill-ring)
         )
  :init ; (helm-mode 1) (helm-top-poll-mode 1)
  (setq helm-mode-line-string nil) ; use default mode-line instead of help string.
  :config
  ;; echo input in header line
  (setq helm-echo-input-in-header-line t)
  (add-hook 'helm-minibuffer-set-up-hook #'helm-hide-minibuffer-maybe)
  ;; helm window position
  (setq helm-split-window-inside-p t)
  (setq helm-mini-default-sources
        '(helm-source-buffers-list
          helm-source-bookmarks
          helm-source-recentf
          helm-source-buffer-not-found))
  (setq helm-input-idle-delay 0.1) ; fix Helm fast respond to input caused failed issue.
  (setq helm-org-headings-fontify t))

;;; [ helm-fuz ] -- Integrate Helm and Fuz.

;; (use-package helm-fuz
;;   :ensure t
;;   :init (helm-fuz-mode 1))


(provide 'init-helm)

;;; init-helm.el ends here
