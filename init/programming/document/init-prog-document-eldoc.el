;;; init-prog-document-eldoc.el --- init for ElDoc
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ ElDoc ] --- show you the argument list of the function call you are currently writing in the echo area.

(use-package eldoc
  :ensure t
  :diminish eldoc-mode
  :init (add-hook 'prog-mode-hook #'eldoc-mode)
  :config
  ;; ElDoc with most `paredit' command.
  ;; whenever the listed commands are used, ElDoc will automatically refresh the minibuffer.
  (eldoc-add-command 'paredit-backward-delete 'paredit-close-round))

;;; [ eldoc-overlay ]  -- display eldoc with contextual documentation overlay.

;; (use-package eldoc-overlay
;;   :ensure t
;;   :defer t
;;   :init (global-eldoc-overlay-mode 1))

;;; [ help-at-pt ] -- local help through the keyboard.

(setq help-at-pt-display-when-idle t)

;;; [ eldoc-eval ] -- Enable eldoc support when minibuffer is in use.

;; (use-package eldoc-eval
;;   :ensure t
;;   :defer t
;;   :commands (eldoc-eval-expression)
;;   :bind ("M-:" . eldoc-eval-expression)
;;   :init (setq eldoc-show-in-mode-line-delay 1))


(provide 'init-prog-document-eldoc)

;;; init-prog-document-eldoc.el ends here
