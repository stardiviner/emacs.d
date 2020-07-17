;;; init-emacs-mode-line.el --- init modeline for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;; (require 'init-powerline)

;;; [ mood-line ] -- a minimal mode-line configuration that aims to replicate some of the features of the doom-modeline.

;; (use-package mood-line
;;   :ensure t
;;   :hook (after-init . mood-line-mode))

;;; [ doom-modeline ] -- A minimal and modern mode-line.

;; (use-package doom-modeline
;;   :ensure t
;;   :custom ((doom-modeline-buffer-file-name-style 'buffer-name)
;;            (doom-modeline-icon t) ; don't use icon will be faster
;;            (doom-modeline-github nil)
;;            ;; Fix the laggy issue, by don't compact font caches during GC.
;;            (inhibit-compacting-font-caches t))
;;   :hook (after-init . doom-modeline-mode))


(provide 'init-emacs-mode-line)

;;; init-emacs-mode-line.el ends here
