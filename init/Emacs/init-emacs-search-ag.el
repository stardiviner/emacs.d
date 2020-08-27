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
  :defer t
  :commands (ag ag-regexp ag-project ag-regexp-project-at-point ag-dired ag-files ag-kill-buffers)
  :custom ((ag-highlight-search t)
           (ag-group-matches t)
           (ag-context-lines nil)
           (ag-reuse-buffers t)
           (ag-reuse-window nil) ; nil, or 't. (I use value `nil' for popwin to capture)
           ;; (ag-arguments)
           )
  :init (add-to-list 'display-buffer-alist '("^\\*ag search\\*" . (display-buffer-below-selected)))
  :config
  (define-key ag-prefix (kbd "a") 'ag)
  (define-key ag-prefix (kbd "r") 'ag-regexp)
  (define-key ag-prefix (kbd "p") 'ag-project) ; `ag-project-files', `ag-project-regexp', `ag-project-dired'
  (define-key ag-prefix (kbd "P") 'ag-regexp-project-at-point)
  (define-key ag-prefix (kbd "d") 'ag-dired)      ; `ag-dired-regexp'
  (define-key ag-prefix (kbd "f") 'ag-files)
  (define-key ag-prefix (kbd "k") 'ag-kill-buffers) ; `ag-kill-other-buffers'

  ;; auto jump to result buffer window.
  (add-hook 'ag-search-finished-hook
            (lambda () (pop-to-buffer next-error-last-buffer)))
  
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
  :bind (:map ag-prefix ("h" . helm-ag) ("M-h" . helm-do-ag))
  :custom ((helm-follow-mode-persistent t)
           (helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
           ;; (helm-ag-command-option "--all-text")
           (helm-ag-insert-at-point 'symbol)))


(provide 'init-emacs-search-ag)

;;; init-emacs-search-ag.el ends here
