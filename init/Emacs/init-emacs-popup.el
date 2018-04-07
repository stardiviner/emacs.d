;;; init-emacs-popup.el --- init popup settings for Emacs.
;;
;;; Commentary:

;;; Code:

;;; use GTK+ tooltips for Emacs widget tooltips.
;; (setq x-gtk-use-system-tooltips nil) ; set to `nil' to fix face un-customized issue.

;;; [ tooltip ]

(use-package tooltip
  :config
  (setq tooltip-reuse-hidden-frame t)

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
  :config
  (setq pos-tip-use-relative-coordinates nil
        pos-tip-border-width 1
        pos-tip-internal-border-width 2
        ;; pos-tip-tab-width nil
        )
  )

;;; [ showtip ]

;; (use-package showtip
;;   :ensure t
;;   :defer t
;;   :config
;;   (setq showtip-timeout 15
;;         showtip-top-adjust 30)
;;   )

;;; [ child-frame ]

;;; [ posframe ] -- Pop a posframe (just a child-frame) at point.

(use-package posframe
  :ensure t)

(use-package ivy-posframe
  :ensure t
  :config
  ;; global replace ivy in minibuffer.
  ;; (setq ivy-display-function #'ivy-posframe-display-at-point)
  ;; (setq ivy-posframe-style 'point)
  ;; (setq ivy-display-function #'ivy-posframe-display-at-window-bottom-left)
  ;; (ivy-posframe-enable)

  ;; replace some specific commands.
  (push '(counsel-org-goto . ivy-posframe-display-at-window-bottom-left) ivy-display-functions-alist)
  
  ;; show fringe to ivy-posframe
  ;; (setq ivy-posframe-parameters
  ;;       '((left-fringe . 5)
  ;;         (right-fringe . 5)))

  ;; add border to ivy-posframe
  ;; (defun ivy-format-function-default (cands)
  ;;   "Transform CANDS into a string for minibuffer."
  ;;   (concat "/-------------------------\n"
  ;;           (ivy--format-function-generic
  ;;            (lambda (str)
  ;;              (concat "|> " (ivy--add-face str 'ivy-current-match) ""))
  ;;            (lambda (str)
  ;;              (concat "|  " str ""))
  ;;            cands
  ;;            "\n")
  ;;           "\n`--------------------------\n"
  ;;           ))

  ;; change ivy-posframe background color.
  (defun my:ivy-posframe-face-setup (theme)
    "Reload customized faces on `circadian' `THEME' toggling."
    (set-face-attribute 'ivy-posframe nil
                        :background (color-darken-name (face-background 'default) 10)
                        :foreground (face-foreground 'default)))
  (add-hook 'circadian-after-load-theme-hook #'my:ivy-posframe-face-setup)
  )


(provide 'init-emacs-popup)

;;; init-emacs-popup.el ends here
