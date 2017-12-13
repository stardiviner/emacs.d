;;; init-my-prog-document-eldoc.el --- init for ElDoc
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ ElDoc ] --- show you the argument list of the function call you are currently writing in the echo area.

(use-package eldoc
  :ensure t
  :config
  ;; (global-eldoc-mode t)

  (dolist (hook
           '(prog-mode-hook
             emacs-lisp-mode-hook
             lisp-interaction-mode-hook
             lisp-mode-hook
             ielm-mode-hook
             ))
    (add-hook hook #'eldoc-mode))

  ;; (set-face-attribute 'eldoc-highlight-function-argument nil
  ;;                     :foreground "deep pink"
  ;;                     :underline t
  ;;                     :weight 'bold)

  ;; ElDoc with most `paredit' command.
  ;; whenever the listed commands are used, ElDoc will automatically refresh the minibuffer.
  (eldoc-add-command 'paredit-backward-delete
                     'paredit-close-round)
  )

;;; [ eldoc-overlay-mode ]  -- display eldoc with contextual documentation overlay.

(use-package eldoc-overlay-mode
  :ensure t
  :config
  (eldoc-overlay-mode 1))


(provide 'init-my-prog-document-eldoc)

;;; init-my-prog-document-eldoc.el ends here
