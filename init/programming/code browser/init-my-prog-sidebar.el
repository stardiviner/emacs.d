;;; init-my-prog-sidebar.el --- init Emacs sidebar for Programming.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ emacs-neotree ]

(use-package neotree
  :ensure t
  :bind ([f8] . neotree)
  :config
  (setq neo-window-position 'left
        neo-window-width 25
        neo-window-fixed-size t
        neo-smart-open t
        neo-auto-indent-point t
        neo-vc-integration nil
        )
  (setq neo-theme (if (and (featurep 'all-the-icons) (display-graphic-p)) 'icons 'classic))
  )



(provide 'init-my-prog-sidebar)

;;; init-my-prog-sidebar.el ends here
