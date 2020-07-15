;;; init-helm.el --- simple configure Helm
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Helm ] -- Helm is an Emacs incremental and narrowing framework.

(use-package helm
  :ensure t
  :defer t
  :custom ((helm-input-idle-delay 0.1) ; fix Helm fast respond to input caused failed issue.
           (helm-echo-input-in-header-line t) ; echo input in header line
           (helm-split-window-inside-p t) ; helm window position
           (helm-ff-keep-cached-candidates nil) ; disable `helm-ff-cache-mode' idle timer
           )
  ;; :bind (([remap execute-extended-command] . helm-M-x)
  ;;        ("M-x" . helm-M-x)
  ;;        ([remap switch-to-buffer] . helm-mini)
  ;;        ([remap yank-pop] . helm-show-kill-ring)
  ;;        ([remap yas-insert-snippet] . helm-yas-complete))
  ;; :hook (after-init . helm-mode)
  :config
  ;; (helm-top-poll-mode 1)
  (add-hook 'helm-minibuffer-set-up-hook #'helm-hide-minibuffer-maybe)
  (setq helm-mini-default-sources
        '(helm-source-buffers-list
          helm-source-bookmarks
          helm-source-recentf
          helm-source-buffer-not-found)))

;;; [ helm-fuz ] -- Integrate Helm and Fuz.

;; (use-package helm-fuz
;;   :ensure t
;;   :init (helm-fuz-mode 1))


(provide 'init-helm)

;;; init-helm.el ends here
