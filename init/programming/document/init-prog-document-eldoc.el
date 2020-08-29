;;; init-prog-document-eldoc.el --- init for ElDoc
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ ElDoc ] --- show you the argument list of the function call you are currently writing in the echo area.

(use-package eldoc
  :ensure t
  :defer t
  :delight eldoc-mode
  :preface (setq global-eldoc-mode t)
  :hook (prog-mode . eldoc-mode)
  :init (global-eldoc-mode -1) ; don't enable `global-eldoc-mode'
  :config
  ;; ElDoc with most `paredit' command.
  ;; whenever the listed commands are used, ElDoc will automatically refresh the minibuffer.
  (eldoc-add-command 'paredit-backward-delete 'paredit-close-round))

;;; [ eldoc-overlay ]  -- display eldoc with contextual documentation overlay.

(use-package eldoc-overlay
  :ensure t
  :delight eldoc-overlay-mode
  :hook (eldoc-mode . eldoc-overlay-mode))

;;; [ eldoc-box ] -- Display eldoc documentation in childframe.

;; (use-package eldoc-box
;;   :ensure t
;;   :delight eldoc-box-hover-at-point-mode
;;   :hook (eldoc-mode . eldoc-box-hover-at-point-mode))

;;; [ help-at-pt ] -- local help through the keyboard.

;; (setq help-at-pt-display-when-idle t)

;;; [ eldoc-eval ] -- Enable eldoc support when minibuffer is in use.

;; (use-package eldoc-eval
;;   :ensure t
;;   :defer t
;;   :commands (eldoc-eval-expression)
;;   :bind ("M-:" . eldoc-eval-expression)
;;   :init (setq eldoc-show-in-mode-line-delay 1))


(provide 'init-prog-document-eldoc)

;;; init-prog-document-eldoc.el ends here
