;;; init-my-emacs-popup.el --- init popup settings for Emacs.
;;
;;; Commentary:

;;; Code:

;;; [ tooltip ]

;;; Usage:
;;
;; - [M-x x-show-tip] ::
;; - (tooltip-show) ::

(use-package tooltip
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

  (set-face-attribute 'tooltip nil
                      :foreground "black" :background "light yellow")

  (tooltip-mode t)
  )


;;; [ popup ]

;; Features:
;;  - Tooltip
;;      `popup-tip'
;;  - Popup Menu
;;      (popup-menu*)
;;  - Popup Cascade Menu
;;      (popup-cascade-menu)
;; e.g. (popup-cascade-menu '(("Top1" "Sub1" "Sub2") "Top2"))
;;      Navigate between menu candidate and cascade menu with [C-f], [C-b]

(use-package popup
  :ensure t
  :config
  
  ;; (set-face-attribute 'popup-face nil
  ;;                     :inherit 'tooltip)

  ;; add some shotcuts in popup menu mode
  (define-key popup-menu-keymap (kbd "M-n") 'popup-next)
  (define-key popup-menu-keymap (kbd "M-p") 'popup-previous)
  (define-key popup-menu-keymap (kbd "M-j") 'popup-select)
  )


;;; [ pos-tip ]

(use-package pos-tip
  :config
  (setq pos-tip-use-relative-coordinates nil
        pos-tip-border-width 1
        pos-tip-internal-border-width 2
        ;; pos-tip-tab-width nil
        )
  )


;;; [ popup-pos-tip ]

;; (use-package popup-pos-tip)


;;; [ showtip ]

(use-package showtip
  :ensure t
  :config
  (setq showtip-timeout 15
        showtip-top-adjust 30)

  (set-face-attribute 'showtip-face nil
                      :inherit 'tooltip)
  )


(provide 'init-my-emacs-popup)

;;; init-my-emacs-popup.el ends here
