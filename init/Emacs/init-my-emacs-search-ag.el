;;; init-my-emacs-search-ag.el --- init for ag
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ silver search (ag) ] -- like ack, but faster.

(use-package ag
  :ensure t
  :defer t
  :commands ag
  :init
  (unless (boundp 'ag-map)
    (define-prefix-command 'ag-map))
  (define-key my-search-prefix (kbd "a") 'ag-map)
  :bind (:map ag-map
              ("a" . ag)
              ("r" . ag-regexp)
              ("p" . ag-regexp-project-at-point) ; `ag', `ag-regexp'
              ("P" . ag-project) ; `ag-project-files', `ag-project-regexp', `ag-project-dired'
              ("d" . ag-dired) ; `ag-dired-regexp'
              ("f" . ag-files)
              ("k" . ag-kill-buffers) ; `ag-kill-other-buffers'
              )
  
  :config
  (setq ag-highlight-search t
        ag-group-matches t
        ;; ag-context-lines nil
        ag-reuse-buffers 't
        ag-reuse-window nil ; nil, or 't. (I use value `nil' for popwin to capture)
        ;; ag-arguments
        ag-context-lines nil
        ag-group-matches t
        )
  
  (set-face-attribute 'ag-hit-face nil
                      :foreground "gray" :background "black")
  (set-face-attribute 'ag-match-face nil
                      :inverse-video nil
                      :foreground "red"
                      :background (color-darken-name (face-background 'default) 5)
                      )
  
  ;; This will auto open search results in other window.
  ;; (add-hook 'ag-mode-hook #'next-error-follow-minor-mode) ; so you can navigate with 'n' & 'p'.
  )


(provide 'init-my-emacs-search-ag)

;;; init-my-emacs-search-ag.el ends here
