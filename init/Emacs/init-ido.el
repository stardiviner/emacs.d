;;; init-ido.el --- init for ido.

;;; Commentary:



;;; Code:

;;; [ ido ]

(require 'ido)

;;; [ ido-vertical-mode ]

(use-package ido-vertical-mode
  :ensure t
  :config
  (setq ido-vertical-define-keys t
        ido-vertical-show-count t)
  (ido-vertical-mode 1)
  )



(provide 'init-ido)

;;; init-ido.el ends here
