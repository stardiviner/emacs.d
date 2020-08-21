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
  :hook ((prog-mode . flycheck-mode-on-safe)
         (org-mode . flycheck-mode-on-safe)
         ;; (after-init . global-flycheck-mode)
         )
  :bind (:map linter-prefix ("!" . flycheck-mode)
              :map flycheck-mode-map
              ("M-g M-n" . flycheck-next-error)
              ("M-g M-p" . flycheck-previous-error)
              ("M-g M-l" . flycheck-list-errors))
  :config
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist
               '("^\\*Flycheck checker\\*" (display-buffer-below-selected)))
  ;; checker `proselint' for `org-mode', `markdown-mode', `gfm-mode'.
  (add-to-list 'flycheck-checkers 'proselint))

;;; [ flycheck-inline ] -- display Flycheck errors inline.

(use-package flycheck-inline
  :ensure t
  :hook (flycheck-mode . flycheck-inline-mode)
  :config
  ;; use `quick-peek' instead of default overlay.
  (defun flycheck-inline-quick-peek (msg &optional pos err)
    (let* ((ov (quick-peek-overlay-ensure-at pos))
           (contents (quick-peek-overlay-contents ov)))
      (setf (quick-peek-overlay-contents ov)
            (concat contents (when contents "\n") msg))
      (quick-peek-update ov)))
  (setq flycheck-inline-display-function #'flycheck-inline-quick-peek
        flycheck-inline-clear-function #'quick-peek-hide))

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
