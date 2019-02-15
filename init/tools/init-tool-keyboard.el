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

(use-package esonify
  :ensure t
  :commands (esonify-mode))


(provide 'init-tool-keyboard)

;;; init-tool-keyboard.el ends here
