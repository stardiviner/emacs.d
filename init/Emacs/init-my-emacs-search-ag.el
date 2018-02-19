;;; init-my-emacs-search-ag.el --- init for ag
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ silver search (ag) ] -- like ack, but faster.

(unless (boundp 'ag-map)
  (define-prefix-command 'ag-map))
(define-key search-prefix (kbd "a") 'ag-map)

(use-package ag
  :ensure t
  :ensure-system-package (ag . "sudo pacman -S --noconfirm the_silver_searcher")
  :defer t
  :init
  (define-key ag-map (kbd "a") 'ag)
  (define-key ag-map (kbd "r") 'ag-regexp)
  (define-key ag-map (kbd "p") 'ag-project) ; `ag-project-files', `ag-project-regexp', `ag-project-dired'
  (define-key ag-map (kbd "P") 'ag-regexp-project-at-point)
  (define-key ag-map (kbd "d") 'ag-dired)      ; `ag-dired-regexp'
  (define-key ag-map (kbd "f") 'ag-files)
  (define-key ag-map (kbd "k") 'ag-kill-buffers) ; `ag-kill-other-buffers'

  (add-to-list 'display-buffer-alist
               '("^\\*ag search\\*" (display-buffer-below-selected)))
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

;;; [ helm-ag ] --- the silver searcher with helm interface.

(use-package helm-ag
  :ensure t
  :defer t
  :init
  (setq helm-follow-mode-persistent t)
  (define-key ag-map (kbd "h") 'helm-ag)
  (define-key ag-map (kbd "M-h") 'helm-do-ag)
  :config
  (setq helm-ag-base-command "ag --nocolor --nogroup --ignore-case"
        ;; helm-ag-command-option "--all-text"
        helm-ag-insert-at-point 'symbol
        )
  )


(provide 'init-my-emacs-search-ag)

;;; init-my-emacs-search-ag.el ends here
