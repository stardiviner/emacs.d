;;; init-my-prog-lint.el --- init Programming Lint

;;; Commentary:



;;; Code:

(unless (boundp 'linter-prefix)
  (define-prefix-command 'linter-prefix))

(add-hook 'prog-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c !") 'linter-prefix)
            (define-key linter-prefix (kbd "!") 'flycheck-buffer)
            (define-key linter-prefix (kbd "b") 'flycheck-buffer)
            ))


;;; [ FlyCheck ] --- modern on-the-fly syntax checking

(use-package flycheck
  :ensure t
  :commands flycheck-mode
  :init
  ;; (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'prog-mode-hook #'flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-g M-n" . flycheck-next-error)
              ("M-g M-p" . flycheck-previous-error)
              ("M-g M-l" . flycheck-list-errors))
  :config
  (setq flycheck-check-syntax-automatically '(save idle-change)
        flycheck-idle-change-delay 5.0)

  ;; [Emacs Lisp]
  ;; To make Flycheck use the current `load-path'.
  ;; Don't error about "free variable" without (require ??).
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; list errors only when has lint errors
  (defun flycheck-list-errors-only-when-errors ()
    "List errors only when has lint errors."
    (if flycheck-current-errors
        (flycheck-list-errors)
      (-when-let (buffer (get-buffer flycheck-error-list-buffer))
        (dolist (window (get-buffer-window-list buffer))
          (quit-window nil window)))))
  ;; (add-hook 'after-save-hook #'flycheck-list-errors-only-when-errors)
  )


;;; [ flycheck-inline ] -- display errors with inline style.

;; (use-package flycheck-inline
;;   :ensure t
;;   :config
;;   (with-eval-after-load 'flycheck
;;     (add-hook 'flycheck-mode-hook #'flycheck-inline-enable))
;;   )


;;; [ flycheck-popup-tip ] -- displaying errors from Flycheck using popup.el.

(use-package flycheck-popup-tip
  :ensure t
  :init
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode))
  )



(provide 'init-my-prog-lint)

;;; init-my-prog-lint.el ends here
