;;; init-my-emacs-mode-line.el --- init modeline for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(setq mode-line-in-non-selected-windows t)


;;; [ window-divider-mode ]

(when (boundp 'window-divider-mode)
  (setq window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1)
  ;; (window-divider-mode 1)
  )

;;; My custom mode-line fragments
;;
;; (require 'init-custom-mode-line)


;;; [ powerline ] -- Rewrite of Powerline.

;; (use-package powerline
;;   :ensure t
;;   :config
;;   (setq powerline-default-separator 'wave)
;;   (powerline-center-theme)
;;   )

;;; [ spaceline ] -- modeline configuration library for powerline.

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)

  (use-package spaceline-all-the-icons
    :ensure t
    :after spaceline
    :config
    (setq anzu-cons-mode-line-p nil)
    
    (setq spaceline-all-the-icons-separator-type 'arrow)
    (setq spaceline-all-the-icons-icon-set-modified 'chain)

    (set-face-attribute 'spaceline-highlight-face nil
                        :background "SlateGrey")

    (spaceline-toggle-all-the-icons-time-off)
    (spaceline-toggle-all-the-icons-buffer-path-off)
    (spaceline-toggle-all-the-icons-eyebrowse-workspace-on)
    (spaceline-toggle-all-the-icons-projectile-on)
    (spaceline-toggle-all-the-icons-hud-on)
    (spaceline-toggle-all-the-icons-modified-on)
    (spaceline-toggle-all-the-icons-process-on)

    (spaceline-helm-mode)
    (spaceline-info-mode)
    
    (spaceline-all-the-icons-theme))
  )


(provide 'init-my-emacs-mode-line)

;;; init-my-emacs-mode-line.el ends here
