;;; init-emacs-popup.el --- init popup settings for Emacs.
;;
;;; Commentary:

;;; Code:

;;; use GTK+ tooltips for Emacs widget tooltips.
;; (setq x-gtk-use-system-tooltips nil) ; set to `nil' to fix face un-customized issue.

;;; [ tooltip ]

(use-package tooltip
  :init (tooltip-mode 1)
  :config
  ;; (setq tooltip-reuse-hidden-frame t)

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

  ;; (tooltip-mode t)
  )


;;; [ popup ]

(use-package popup
  :ensure t
  :defer t
  :bind (:map popup-menu-keymap
              ("M-n" . popup-next)
              ("M-p" . popup-previous)
              ("M-j" . popup-select)))


;;; [ pos-tip ]

(use-package pos-tip
  :ensure t
  :defer t
  :preface (setq x-gtk-use-system-tooltips nil)
  :config
  (setq pos-tip-border-width 1
        pos-tip-internal-border-width 2)
  (set-face-attribute 'tooltip nil
                      :family "Hack")
  )

;;; [ showtip ]

;;; [ child-frame ]

;;; [ posframe ] -- Pop a posframe (just a child-frame) at point.

(use-package posframe
  :ensure t)


(provide 'init-emacs-popup)

;;; init-emacs-popup.el ends here
