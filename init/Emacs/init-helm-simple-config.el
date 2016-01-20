;;; init-helm-simple-config.el --- simple configure Helm
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Helm appearance ]

(use-package helm
  :config
  (helm-autoresize-mode t)

  (setq helm-full-frame nil ; use current window as popup. helm popup window position.
        helm-always-two-windows t
        helm-autoresize-max-height 25
        helm-autoresize-min-height 6
        ;; display
        ;; helm-display-function 'popwin:pop-to-buffer
        ;; split
        helm-split-window-in-side-p t ; force split inside selected window.
        helm-split-window-default-side 'below
        ;; helm-split-window-preferred-function
        ;; 'helm-split-window-default-fn, 'popwin:pop-to-buffer
        ;; helm-split-window-preferred-function 'popwin:pop-to-buffer
        helm-quick-update t ; do not display invisible candidates
        ;; helm-scroll-amount 8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-case-fold-search t
        helm-buffers-fuzzy-matching nil
        helm-recentf-fuzzy-match nil
        helm-move-to-line-cycle-in-source nil ; nil: not just cycling current limited candicates section.
        ;; find-file
        helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
        helm-ff-file-name-history-use-recentf t ; use recentf
        helm-ff-transformer-show-only-basename t ; show basename in find file candidates. toggled by C-].
        ;; helm-sources-using-default-as-input
        )

  ;; [ Helm faces ]
  ;; FIXME: this does not apply.
  (set-face-attribute 'helm-match nil
                      ;; 1. dim already matched, leave eye on the un-matched strings.
                      :foreground "dim gray"
                      :background (color-darken-name (face-background 'default) 5)
                      ;; 2. highlight/underline already matched
                      ;; :foreground "white"
                      ;; :underline t
                      ;; :weight 'normal
                      )
  (set-face-attribute 'helm-selection nil
                      ;; 1. different font
                      ;; :family "Comic Sans MS" :weight 'normal :height 1.0 :slant 'italic
                      ;; 2. box selected
                      ;; :background "#004A5D" :foreground "white"
                      ;; :box '(:color "cyan" :line-width -1)
                      ;; :underline nil
                      ;; 3. tomato color box
                      :background "#333333" :foreground "white"
                      :box '(:color "tomato" :line-width -1 :style nil)
                      :underline nil :weight 'normal
                      ;; 4. darker background percent 5%
                      ;; :inherit nil
                      ;; :inverse-video nil
                      ;; :background (color-darken-name (face-background 'default) 5)
                      ;; :underline '(:color "dark red")
                      )

  (set-face-attribute 'helm-header nil
                      :reverse-video nil
                      :foreground "gray" :background "black"
                      :weight 'bold)
  (set-face-attribute 'helm-source-header nil
                      :foreground "green" :background "#222222"
                      :overline "yellow" :weight 'bold
                      ;; :family "DejaVu Sans Mono" :height 1.0
                      )
  (set-face-attribute 'helm-prefarg nil
                      :foreground "cyan")
  (set-face-attribute 'helm-action nil
                      :inverse-video nil
                      :foreground "orange"
                      :underline nil
                      )
  (set-face-attribute 'helm-separator nil
                      :foreground "cyan")
  (set-face-attribute 'helm-visible-mark nil
                      :foreground "black" :background "green yellow")

  ;; [ Helm keybindings ]
  (define-key helm-map (kbd "<tab>") 'helm-select-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal.
  (define-key helm-map (kbd "C-j") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "<RET>") 'helm-maybe-exit-minibuffer)

  ;; NOTE: this cause helm-dash open menu candidate error.
  ;; (define-key helm-map (kbd "<return>") 'helm-confirm-and-exit-minibuffer)
  )



(provide 'init-helm-simple-config)

;;; init-helm-simple-config.el ends here
