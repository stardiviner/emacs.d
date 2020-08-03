;;; init-tool-keyboard.el --- init for Keyboard
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ selectric-mode ] -- Make your Emacs sound like a proper typewriter.

;; (use-package selectric-mode
;;   :ensure t
;;   :commands selectric-mode
;;   :init (selectric-mode 1))

;;; [ esonify ] -- An Emacs extension that sonifies your code.

;; (use-package esonify
;;   :ensure t
;;   :commands (esonify-mode))

;;; [ rainbow-fart ] -- 程序猿鼓励师

(use-package rainbow-fart
  :ensure t
  :delight rainbow-fart-mode
  :commands (rainbow-fart-mode)
  :custom ((rainbow-fart-keyword-interval nil))
  :init (rainbow-fart-mode 1))

(provide 'init-tool-keyboard)

;;; init-tool-keyboard.el ends here
