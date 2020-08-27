;;; init-powerline.el --- init Emacs mode-line with powerline.

;;; Commentary:



;;; Code:

;;; [ powerline ] -- Rewrite of Powerline.

(use-package powerline
  :ensure t
  :config
  (setq powerline-default-separator 'wave)
  (powerline-center-theme)
  )

;;; [ spaceline ] -- modeline configuration library for powerline.

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)

  (use-package spaceline-all-the-icons
    :ensure t
    :after spaceline
    :custom ((anzu-cons-mode-line-p nil)
             (spaceline-all-the-icons-separator-type 'arrow)
             (spaceline-all-the-icons-icon-set-modified 'chain))
    :custom-face (spaceline-highlight-face (( t(:background "LightSteelBlue"))))
    :config
    (spaceline-toggle-all-the-icons-time-off)
    (spaceline-toggle-all-the-icons-buffer-path-off)
    (spaceline-toggle-all-the-icons-eyebrowse-workspace-on)
    (spaceline-toggle-all-the-icons-projectile-on)
    (spaceline-toggle-all-the-icons-hud-on)
    (spaceline-toggle-all-the-icons-modified-on)
    (spaceline-toggle-all-the-icons-process-on)

    ;; custom segments
    ;; TODO:
    ;; (spaceline-all-the-icons-theme 'segment-symbol "string"
    ;;                                'etc)

    (spaceline-helm-mode)
    (spaceline-info-mode)
    
    (spaceline-all-the-icons-theme))
  )


(provide 'init-powerline)

;;; init-powerline.el ends here
