;;; init-pass.el --- init for pass

;;; Commentary:



;;; Code:

(unless (boundp 'password-prefix)
  (define-prefix-command 'password-prefix))
(define-key 'my-tools-prefix (kbd "P") 'password-prefix)

;;; [ password-store ] -- password store (pass) support.

(use-package password-store
  :ensure t)

;;; [ auth-password-store ] -- integrate Emacs' `auth-source' with `password-store'.

;; (use-package auth-password-store
;;   :ensure t
;;   :config
;;   ;; Emacs < 26
;;   ;; (auth-pass-enable)
;;   ;; Emacs >= 26
;;   (auth-source-pass-enable)
;;   )

;;; [ pass ] -- major mode for password-store.

(use-package pass
  :ensure t)

;;; [ passmm ] -- minor mode that uses `Dired' to display all password files from the password-store (pass).

(use-package passmm
  :ensure t)

;;; [ helm-pass ] -- Helm interface for pass.

;; (use-package helm-pass
;;   :ensure t
;;   :commands (helm-pass))

;;; [ ivy-pass ] -- Ivy interface for pass.

(use-package ivy-pass
  :ensure t
  :commands (ivy-pass)
  :bind (:map password-prefix
              ("P" . ivy-pass)))



(provide 'init-pass)

;;; init-pass.el ends here
