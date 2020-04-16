;;; init-emacs-popup.el --- init popup settings for Emacs.
;;
;;; Commentary:

;;; Code:

;;; use GTK+ tooltips for Emacs widget tooltips.
;; (setq x-gtk-use-system-tooltips nil) ; set to `nil' to fix face un-customized issue.

;;; [ tooltip ]

;;; [ popup ]

(use-package popup
  :ensure t
  :defer t
  :bind (:map popup-menu-keymap
              ("M-n" . popup-next)
              ("M-p" . popup-previous)
              ("M-j" . popup-select)))

;;; [ posframe ] -- Pop a posframe (just a child-frame) at point.

(use-package posframe
  :ensure t
  :defer t
  :init (setq posframe-mouse-banish t))


(provide 'init-emacs-popup)

;;; init-emacs-popup.el ends here
