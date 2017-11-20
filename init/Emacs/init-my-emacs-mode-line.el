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
(require 'init-custom-mode-line)


;; (require 'init-powerline)


(provide 'init-my-emacs-mode-line)

;;; init-my-emacs-mode-line.el ends here
