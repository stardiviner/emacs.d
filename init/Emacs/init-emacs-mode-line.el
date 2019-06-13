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

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :init (setq doom-modeline-buffer-file-name-style 'buffer-name
              doom-modeline-icon t ; don't use icon will be faster
              doom-modeline-github nil
              doom-modeline-irc nil
              ;; Fix the laggy issue, by don't compact font caches during GC.
              inhibit-compacting-font-caches t)
  :config
  ;; remove `workspace-name' segment
  (doom-modeline-def-modeline 'remove-workspace
    '(bar window-number modals matches buffer-info remote-host buffer-position parrot selection-info)
    '(objed-state misc-info persp-name fancy-battery irc mu4e github debug lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker))
  (add-hook 'doom-modeline-mode-hook
            #'(lambda () (doom-modeline-set-modeline 'remove-workspace))))


(provide 'init-emacs-mode-line)

;;; init-emacs-mode-line.el ends here
