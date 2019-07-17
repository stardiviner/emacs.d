;;; init-prog-lint.el --- init Programming Lint

;;; Commentary:



;;; Code:

(unless (boundp 'linter-prefix)
  (define-prefix-command 'linter-prefix))

(add-hook 'prog-mode-hook
          (lambda () (local-set-key (kbd "C-c !") 'linter-prefix)))

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
  :commands flycheck-mode
  :preface (setq flycheck-check-syntax-automatically '(save)
                 flycheck-global-modes '(not emacs-lisp-mode clojure-mode lisp-mode))
  ;; NOTE: ONLY enable `flycheck-mode' MANUALLY. automatically checking will
  ;; cause high CPU. especially big source code file.
  :init (add-hook 'prog-mode-hook #'flycheck-mode-on-safe)
  :bind (:map linter-prefix ("!" . flycheck-mode)
              :map flycheck-mode-map
              ("M-g M-n" . flycheck-next-error)
              ("M-g M-p" . flycheck-previous-error)
              ("M-g M-l" . flycheck-list-errors))
  :config
  ;; [Emacs Lisp]
  ;; To make Flycheck use the current `load-path'.
  ;; Don't error about "free variable" without (require ??).
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (add-to-list 'display-buffer-alist
               '("^\\*Flycheck errors\\*" (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist
               '("^\\*Flycheck checker\\*" (display-buffer-below-selected)))
  ;; checker `proselint' for `org-mode', `markdown-mode', `gfm-mode'.
  (add-to-list 'flycheck-checkers 'proselint))

;;; [ flycheck-inline ] -- display Flycheck errors inline.

(use-package flycheck-inline
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'flycheck-inline-mode))

;;; [ flycheck-popup-tip ] -- displaying errors from Flycheck using popup.el.

;; (use-package flycheck-popup-tip
;;   :ensure t
;;   :defer t
;;   :after flycheck
;;   :init
;;   (add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode))

;;; [ flycheck-posframe ] -- Show flycheck error messages using posframe.el

;; (use-package flycheck-posframe
;;   :ensure t
;;   :init (add-hook 'flycheck-mode-hook 'flycheck-posframe-mode)
;;   :config
;;   (set-face-attribute 'flycheck-posframe-background-face nil
;;                       :background (cl-case (alist-get 'background-mode (frame-parameters))
;;                                     ('light
;;                                      (color-darken-name (face-background 'default) 10))
;;                                     ('dark
;;                                      (color-lighten-name (face-background 'default) 5)))
;;                       )
;;   )



(provide 'init-prog-lint)

;;; init-prog-lint.el ends here
