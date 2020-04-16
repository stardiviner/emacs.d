;;; init-emacs-mode-line.el --- init modeline for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(setq mode-line-in-non-selected-windows t)

;; (require 'init-custom-mode-line)
;; (require 'init-powerline)

;;; [ mood-line ] -- a minimal mode-line configuration that aims to replicate some of the features of the doom-modeline.

;; (use-package mood-line
;;   :ensure t
;;   :hook (after-init . mood-line-mode))

;;; [ doom-modeline ] -- A minimal and modern mode-line.

;; (use-package doom-modeline
;;   :ensure t
;;   :hook (after-init . doom-modeline-mode)
;;   :init (setq doom-modeline-buffer-file-name-style 'buffer-name
;;               doom-modeline-icon t ; don't use icon will be faster
;;               doom-modeline-github nil
;;               doom-modeline-irc nil
;;               ;; Fix the laggy issue, by don't compact font caches during GC.
;;               inhibit-compacting-font-caches t))


(provide 'init-emacs-mode-line)

;;; init-emacs-mode-line.el ends here
