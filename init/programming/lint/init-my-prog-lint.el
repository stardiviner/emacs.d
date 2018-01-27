;;; init-my-prog-lint.el --- init Programming Lint

;;; Commentary:



;;; Code:

(unless (boundp 'linter-prefix)
  (define-prefix-command 'linter-prefix))

(add-hook 'prog-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c !") 'linter-prefix)))


;;; [ FlyCheck ] --- modern on-the-fly syntax checking

(use-package flycheck
  :ensure t
  :commands flycheck-mode
  :preface
  (setq flycheck-check-syntax-automatically '(save)
        flycheck-idle-change-delay 5.0)
  ;; (add-hook 'after-init-hook #'global-flycheck-mode)
  ;; (add-hook 'prog-mode-hook #'flycheck-mode)
  :bind (:map linter-prefix
              ;; ("!" . flycheck-mode)
              ;; ("b" . flycheck-buffer)
              :map flycheck-mode-map
              ("M-g M-n" . flycheck-next-error)
              ("M-g M-p" . flycheck-previous-error)
              ("M-g M-l" . flycheck-list-errors))
  :config
  ;; [Emacs Lisp]
  ;; To make Flycheck use the current `load-path'.
  ;; Don't error about "free variable" without (require ??).
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; step 1: start flycheck after saving buffer.
  (defun my/flycheck-manually ()
    "Only trigger flycheck manually after save buffer."
    (interactive)
    (flycheck-mode 1) ; before check buffer, need to enable `flycheck-mode'.
    (flycheck-buffer))
  (add-hook 'after-save-hook #'my/flycheck-manually)
  ;; step 2: list out errors.
  ;; list errors only when has lint errors
  (defun flycheck-list-errors-only-when-errors ()
    "List errors only when has lint errors."
    (if (not (null flycheck-current-errors))
        (unless (get-buffer-window (get-buffer flycheck-error-list-buffer))
          (flycheck-list-errors)
          (switch-to-buffer-other-window flycheck-error-list-buffer))
      (quit-window nil (flycheck-get-error-list-window-list))))
  (add-hook 'flycheck-after-syntax-check-hook #'flycheck-list-errors-only-when-errors)
  (add-to-list 'display-buffer-alist
               '("^\\*Flycheck errors\\*" (display-buffer-below-selected)))
  ;; step 3: disable flycheck.
  (defun my/flycheck-disable ()
    "Disable again to remove `post-command-hook' timers."
    (flycheck-mode -1))
  (add-hook 'flycheck-after-syntax-check-hook #'my/flycheck-disable t)
  )


;;; [ flycheck-inline ] -- display errors with inline style.

;; (use-package flycheck-inline
;;   :ensure t
;;   :config
;;   (with-eval-after-load 'flycheck
;;     (add-hook 'flycheck-mode-hook #'flycheck-inline-enable))
;;   )


;;; [ flycheck-popup-tip ] -- displaying errors from Flycheck using popup.el.

;; (use-package flycheck-popup-tip
;;   :ensure t
;;   :init
;;   (with-eval-after-load 'flycheck
;;     (add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode))
;;   )



(provide 'init-my-prog-lint)

;;; init-my-prog-lint.el ends here
