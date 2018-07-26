;;; init-emacs-search-ag.el --- init for ag
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ silver search (ag) ] -- like ack, but faster.

(unless (boundp 'ag-prefix)
  (define-prefix-command 'ag-prefix))
(define-key search-prefix (kbd "a") 'ag-prefix)

(use-package ag
  :ensure t
  :ensure-system-package (ag . "sudo pacman -S --noconfirm the_silver_searcher")
  :defer t
  :init
  (define-key ag-prefix (kbd "a") 'ag)
  (define-key ag-prefix (kbd "r") 'ag-regexp)
  (define-key ag-prefix (kbd "p") 'ag-project) ; `ag-project-files', `ag-project-regexp', `ag-project-dired'
  (define-key ag-prefix (kbd "P") 'ag-regexp-project-at-point)
  (define-key ag-prefix (kbd "d") 'ag-dired)      ; `ag-dired-regexp'
  (define-key ag-prefix (kbd "f") 'ag-files)
  (define-key ag-prefix (kbd "k") 'ag-kill-buffers) ; `ag-kill-other-buffers'

  (add-to-list 'display-buffer-alist
               '("^\\*ag search\\*" (display-buffer-reuse-window display-buffer-below-selected)))
  :config
  (setq ag-highlight-search t
        ag-group-matches t
        ag-context-lines nil
        ag-reuse-buffers t
        ag-reuse-window nil ; nil, or 't. (I use value `nil' for popwin to capture)
        ;; ag-arguments
        )
  
  ;; This will auto open search results in other window.
  ;; (add-hook 'ag-mode-hook #'next-error-follow-minor-mode) ; so you can navigate with 'n' & 'p'.
  )

;;; [ wgrep-ag ]

(use-package wgrep-ag
  :ensure t
  :defer t
  :after ag
  :init (add-hook 'ag-mode-hook 'wgrep-ag-setup))

;;; [ helm-ag ] --- the silver searcher with helm interface.

(use-package helm-ag
  :ensure t
  :defer t
  :init
  (setq helm-follow-mode-persistent t)
  (define-key ag-prefix (kbd "h") 'helm-ag)
  (define-key ag-prefix (kbd "M-h") 'helm-do-ag)
  :config
  (setq helm-ag-base-command "ag --nocolor --nogroup --ignore-case"
        ;; helm-ag-command-option "--all-text"
        helm-ag-insert-at-point 'symbol
        )
  )


(provide 'init-emacs-search-ag)

;;; init-emacs-search-ag.el ends here
