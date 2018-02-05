;;; init-my-prog-document-eldoc.el --- init for ElDoc
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ ElDoc ] --- show you the argument list of the function call you are currently writing in the echo area.

(use-package eldoc
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'eldoc-mode)
  :config
  ;; ElDoc with most `paredit' command.
  ;; whenever the listed commands are used, ElDoc will automatically refresh the minibuffer.
  (eldoc-add-command 'paredit-backward-delete 'paredit-close-round)
  )

;;; [ eldoc-overlay ]  -- display eldoc with contextual documentation overlay.

;; (use-package eldoc-overlay
;;   :ensure t
;;   :config
;;   (global-eldoc-overlay-mode 1))


(provide 'init-my-prog-document-eldoc)

;;; init-my-prog-document-eldoc.el ends here
