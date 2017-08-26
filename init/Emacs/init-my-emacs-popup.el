;;; init-my-emacs-popup.el --- init popup settings for Emacs.
;;
;;; Commentary:

;;; Code:

;; (setq x-gtk-use-system-tooltips t)

;;; [ tooltip ]

(use-package tooltip
  :defer t
  :config
  ;; (setq-default tooltip-delay 0.3         ; default 0.7
  ;;               tooltip-hide-delay 10     ; default 10
  ;;               tooltip-short-delay 0.1   ; default 0.1
  ;;               ;; tooltip-functions '(tooltip-help-tips)
  ;;               ;; tooltip-hide-time nil
  ;;               tooltip-x-offset 5
  ;;               tooltip-y-offset +20
  ;;               tooltip-frame-parameters '((name . "tooltip")
  ;;                                          (internal-border-width . 2)
  ;;                                          (border-width . 2))
  ;;               )

  (tooltip-mode t)
  )


;;; [ popup ]

(use-package popup
  :ensure t
  :defer t
  :bind (:map popup-menu-keymap
              ("M-n" . popup-next)
              ("M-p" . popup-previous)
              ("M-j" . popup-select))
  :config
  (set-face-attribute 'popup-face nil
                      :inherit 'tooltip
                      :foreground "#073642"
                      :background "light yellow")
  )


;;; [ pos-tip ]

(use-package pos-tip
  :ensure t
  :defer t
  :config
  (setq pos-tip-use-relative-coordinates nil
        pos-tip-border-width 1
        pos-tip-internal-border-width 2
        ;; pos-tip-tab-width nil
        )

  (setq pos-tip-foreground-color "#073642"
        pos-tip-background-color "light yellow")
  )


;;; [ popup-pos-tip ]

;; (use-package popup-pos-tip)


;;; [ showtip ]

(use-package showtip
  :ensure t
  :config
  (setq showtip-timeout 15
        showtip-top-adjust 30)
  )


(provide 'init-my-emacs-popup)

;;; init-my-emacs-popup.el ends here
