;;; init-helm.el --- simple configure Helm
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Helm appearance ]

(use-package helm
  :ensure t
  :bind (("C-x f" . helm-for-files)
         ("C-x c c" . helm-colors)
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
  (helm-autoresize-mode t)

  (setq helm-full-frame nil
        helm-always-two-windows t
        helm-autoresize-max-height 25
        helm-autoresize-min-height 6
        ;; helm-display-function 'popwin:pop-to-buffer
        helm-split-window-in-side-p t
        helm-split-window-default-side 'below
        ;; helm-split-window-preferred-function
        helm-quick-update nil
        helm-case-fold-search t
        helm-buffers-fuzzy-matching nil
        helm-recentf-fuzzy-match nil
        helm-move-to-line-cycle-in-source nil
        ;; find-file
        helm-ff-search-library-in-sexp nil
        helm-ff-file-name-history-use-recentf nil
        helm-ff-transformer-show-only-basename t
        ;; helm-sources-using-default-as-input
        )

  ;; [ Helm faces ]
  (set-face-attribute 'helm-selection nil
                      :inherit nil
                      ;; 1. box selected
                      ;; :background "#004A5D" :foreground "white"
                      ;; :box '(:color "cyan" :line-width -1)
                      ;; :underline nil
                      ;; 2. darker background
                      :inverse-video nil
                      :foreground nil
                      :background (color-darken-name (face-background 'default) 4)
                      )
  ;; (set-face-attribute 'helm-match nil
  ;;                     :inherit nil
  ;;                     :foreground "white"
  ;;                     :background nil
  ;;                     :box '(:color "cyan" :line-width -1)
  ;;                     )
  ;; (set-face-attribute 'helm-header nil
  ;;                     :reverse-video nil
  ;;                     :foreground "gray" :background "black"
  ;;                     :weight 'bold)
  ;; (set-face-attribute 'helm-source-header nil
  ;;                     :foreground "green" :background "#222222"
  ;;                     :overline "yellow" :weight 'bold
  ;;                     )
  ;; (set-face-attribute 'helm-prefarg nil
  ;;                     :foreground "cyan")
  ;; (set-face-attribute 'helm-action nil
  ;;                     :inverse-video nil
  ;;                     :foreground "orange"
  ;;                     :underline nil
  ;;                     )
  ;; (set-face-attribute 'helm-separator nil
  ;;                     :foreground "cyan")
  ;; (set-face-attribute 'helm-visible-mark nil
  ;;                     :foreground "black" :background "green yellow")
  )



(provide 'init-helm)

;;; init-helm.el ends here
