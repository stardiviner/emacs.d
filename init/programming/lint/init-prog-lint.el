;;; init-prog-lint.el --- init Programming Lint

;;; Commentary:



;;; Code:

(unless (boundp 'linter-prefix)
  (define-prefix-command 'linter-prefix))

(define-key prog-mode-map (kbd "C-c !") 'linter-prefix)

;;; [ flymake ] -- A universal on-the-fly syntax checker.

;; (use-package flymake
;;   :ensure t
;;   :init (add-hook 'prog-mode-hook #'flymake-mode-on)
;;   :bind (:map linter-prefix ("!" . flymake-mode)
;;               :map flymake-mode-map
;;               ("M-g M-n" . flymake-goto-next-error)
;;               ("M-g M-p" . flymake-goto-prev-error)))

;;; [ FlyCheck ] --- modern on-the-fly syntax checking

(use-package flycheck
  :ensure t
  :defer t
  :commands (flycheck-mode flycheck-next-error flycheck-previous-error flycheck-list-errors)
  :custom ((flycheck-global-modes '(not emacs-lisp-mode
                                        text-mode markdown-mode org-mode
                                        lisp-mode clojure-mode
                                        sql-mode))
           (flycheck-check-syntax-automatically '(save idle-change new-line))
           (flycheck-display-errors-delay 0.3)
           ;; let flycheck use the current `load-path'.
           ;; don't error about "free variable" without (require ??).
           (flycheck-emacs-lisp-load-path 'inherit))
  ;; NOTE: ONLY enable `flycheck-mode' MANUALLY. automatically checking will
  ;; cause high CPU. especially big source code file.
  :hook ((after-init . global-flycheck-mode)
         ;; (prog-mode . flycheck-mode-on-safe)
         ;; (org-mode . flycheck-mode-on-safe)
         )
  :bind (:map linter-prefix ("!" . flycheck-mode)
              :map flycheck-mode-map
              ("M-g M-n" . flycheck-next-error)
              ("M-g M-p" . flycheck-previous-error)
              ("M-g M-l" . flycheck-list-errors))
  :init (add-to-list 'display-buffer-alist '("^\\*Flycheck .*\\*" . (display-buffer-below-selected)))
  ;; checker `proselint' for `org-mode', `markdown-mode', `gfm-mode'.
  :config (add-to-list 'flycheck-checkers 'proselint))

;;; [ flycheck-inline ] -- display Flycheck errors inline.

(use-package flycheck-inline
  :ensure t
  :hook (flycheck-mode . flycheck-inline-mode))



(provide 'init-prog-lint)

;;; init-prog-lint.el ends here
