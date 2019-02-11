;;; init-emacs-mode-line.el --- init modeline for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(setq mode-line-in-non-selected-windows t)

;; (require 'init-custom-mode-line)
;; (require 'init-powerline)

;;; [ doom-modeline ] -- A minimal and modern mode-line.

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :init (setq doom-modeline-buffer-file-name-style 'buffer-name
              doom-modeline-icon nil ; don't use icon will be faster
              doom-modeline-github nil))


(provide 'init-emacs-mode-line)

;;; init-emacs-mode-line.el ends here
