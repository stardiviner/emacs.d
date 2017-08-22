;;; init-helm.el --- simple configure Helm
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Helm appearance ]

(use-package helm
  :ensure t
  ;; :diminish helm-mode
  ;; :bind (
  ;;        ("M-x" . helm-M-x)
  ;;        ("M-y" . helm-show-kill-ring)
  ;;        ("C-x f" . helm-for-files)
  ;;        ("C-x c /" . helm-find)
  ;;        ("C-x c c" . helm-colors)
  ;;        ("C-x c C-r" . helm-grep-it)
  ;;        )
  :config
  (require 'helm)
  (require 'helm-config)

  ;; (helm-mode 1)
  ;; (helm-mode -1)
  
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

  ;; Grep (grep, ag, pt, rg)
  (require 'helm-grep)
  (setq helm-grep-ag-command
        "rg --smart-case --no-heading --line-number %s %s %s")
  ;; (setq helm-grep-ag-pipe-cmd-switches '(" --color-always --colors 'match:fg:black'" "--colors 'match:bg:yellow'"))
  ;; (setq helm-grep-ag-command
  ;;       "rg --color=always --colors 'match:fg:black' --colors 'match:bg:yellow' --smart-case --no-heading --line-number %s %s %s")
  ;; [ wgrep ]
  (define-key helm-grep-mode-map (kbd "C-c C-p") 'wgrep-change-to-wgrep-mode)

  ;; Helm internal keybindings
  ;; (define-key helm-map ("<tab>") . helm-selection-action)
  ;; (define-key helm-map ("C-j") . helm-execute-persistent-action)
  ;; (define-key helm-map ("<RET>") . helm-maybe-exit-minibuffer)
  ;; NOTE: this cause helm-dash open menu candidate error.
  ;; (define-key helm-map ("<return>") . helm-confirm-and-exit-minibuffer)
  )



(provide 'init-helm)

;;; init-helm.el ends here
