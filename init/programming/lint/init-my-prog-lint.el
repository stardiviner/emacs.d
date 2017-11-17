;;; init-my-prog-lint.el --- init Programming Lint

;;; Commentary:



;;; Code:

(unless (boundp 'my-prog-lint-map)
  (define-prefix-command 'my-prog-lint-map))

(add-hook 'prog-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c !") 'my-prog-lint-map)
            (define-key my-prog-lint-map (kbd "!") 'flycheck-buffer)
            (define-key my-prog-lint-map (kbd "b") 'flycheck-buffer)
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
  (setq-default flycheck-temp-prefix ".flycheck")
  (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change) ; new-line
        flycheck-idle-change-delay 5.0
        ;; flycheck-display-errors-delay 0.9
        flycheck-highlighting-mode 'symbols
        flycheck-indication-mode 'left-fringe
        ;; 'flycheck-fringe-bitmap-double-arrow
        ;; flycheck-display-errors-function 'flycheck-display-error-messages
        flycheck-standard-error-navigation t ; [M-g n/p]
        flycheck-deferred-syntax-check nil
        ;; flycheck-mode-line '(:eval (flycheck-mode-line-status-text))
        flycheck-completing-read-function 'completing-read
        )

  ;; [Emacs Lisp]
  ;; To make Flycheck use the current `load-path'.
  ;; Don't error about "free variable" without (require ??).
  (setq flycheck-emacs-lisp-initialize-packages 'auto
        flycheck-emacs-lisp-load-path 'inherit
        flycheck-emacs-lisp-package-user-dir nil
        )

  ;; [Ruby]
  (setq flycheck-ruby-executable "rubocop"
        ;; flycheck-rubocop-lint-only t
        )

  ;; [Clang]
  ;; flycheck-clang-definitions
  ;; flycheck-clang-include-path
  ;; flycheck-clang-includes
  ;; flycheck-clang-language-standard
  ;; flycheck-clang-no-rtti
  ;; flycheck-clang-standard-library
  ;; flycheck-clang-warnings
  ;; flycheck-cppcheck-checks

  (set-face-attribute 'flycheck-info nil
                      :underline '(:color "forest green" :style wave))
  (set-face-attribute 'flycheck-warning nil
                      :underline '(:color "DarkGoldenrod" :style wave))
  (set-face-attribute 'flycheck-error nil
                      :underline '(:color "red" :style wave))
  (set-face-attribute 'flycheck-fringe-info nil
                      :foreground "forest green")
  (set-face-attribute 'flycheck-fringe-warning nil
                      :foreground "DarkSlateBlue")
  (set-face-attribute 'flycheck-fringe-error nil
                      :foreground "red")


  ;; list errors only when has lint errors
  (defun flycheck-list-errors-only-when-errors ()
    "List errors only when has lint errors."
    (if flycheck-current-errors
        (flycheck-list-errors)
      (-when-let (buffer (get-buffer flycheck-error-list-buffer))
        (dolist (window (get-buffer-window-list buffer))
          (quit-window nil window)))))
  ;; (add-hook 'before-save-hook #'flycheck-list-errors-only-when-errors)
  )



;;; [ flycheck-ineline ] -- display errors with inline style.

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
