;;; init-helm.el --- simple configure Helm
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Helm appearance ]

(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (
         ;; ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x f" . helm-for-files)
         ("C-x c /" . helm-find)
         ("C-x c c" . helm-colors)
         ("C-x c C-r" . helm-grep-it)
         :map helm-map
         ("<tab>" . helm-selection-action)
         ("C-z" . helm-selection-action)
         ("C-i" . helm-execute-persistent-action) ; make TAB works in terminal.
         ("C-j" . helm-execute-persistent-action)
         ("<RET>" . helm-maybe-exit-minibuffer)
         ;; NOTE: this cause helm-dash open menu candidate error.
         ;; ("<return>" . helm-confirm-and-exit-minibuffer)
         )
  :config
  (require 'helm-config)
  
  (helm-autoresize-mode t)
  (setq helm-full-frame nil
        helm-always-two-windows nil
        helm-split-window-in-side-p t
        helm-split-window-default-side 'below
        ;; helm-split-window-preferred-function
        ;; helm-display-function 'popwin:pop-to-buffer
        ;; helm-autoresize-max-height 25
        helm-autoresize-min-height 6
        helm-case-fold-search t
        ;; fuzzy matching
        helm-mode-fuzzy-match t
        helm-completion-in-region-fuzzy-match t
        helm-buffers-fuzzy-matching nil
        helm-recentf-fuzzy-match nil
        helm-move-to-line-cycle-in-source nil
        ;; find-file
        helm-ff-search-library-in-sexp nil
        helm-ff-file-name-history-use-recentf nil
        helm-ff-transformer-show-only-basename t
        ;; helm-sources-using-default-as-input
        )

  ;; (helm-mode 1)
  )



(provide 'init-helm)

;;; init-helm.el ends here
