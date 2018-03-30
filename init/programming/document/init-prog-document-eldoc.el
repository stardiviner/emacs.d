;;; init-prog-document-eldoc.el --- init for ElDoc
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ ElDoc ] --- show you the argument list of the function call you are currently writing in the echo area.

(use-package eldoc
  :ensure t
  :init
  ;; FIX ob-shell eldoc error.
  (global-eldoc-mode -1)
  (dolist (hook '(prog-mode-hook
                  ))
    (add-hook hook
              #'(lambda ()
                  (unless (or (eq major-mode 'sh-mode)
                              (eq major-mode 'org-mode))
                    (eldoc-mode 1)))))
  :config
  (setq eldoc-idle-delay 0.1)
  ;; ElDoc with most `paredit' command.
  ;; whenever the listed commands are used, ElDoc will automatically refresh the minibuffer.
  (eldoc-add-command 'paredit-backward-delete 'paredit-close-round)
  )

;;; [ eldoc-overlay ]  -- display eldoc with contextual documentation overlay.

;; (use-package eldoc-overlay
;;   :ensure t
;;   :init
;;   (global-eldoc-overlay-mode 1))


(provide 'init-prog-document-eldoc)

;;; init-prog-document-eldoc.el ends here
