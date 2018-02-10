;;; init-my-emacs-encrypt.el --- init Emacs encrypt & decrypt

;;; Commentary:


;;; Code:

(unless (boundp 'tools-prefix)
  (define-prefix-command 'tools-prefix))
(unless (boundp 'encrypt-prefix)
  (define-prefix-command 'encrypt-prefix))
(define-key tools-prefix (kbd "M-e") 'encrypt-prefix)

;;; [ password ]

(setq password-cache-expiry nil) ; don't expire password cache.


;;; [ Auth-Source ]

(add-to-list 'auth-sources (concat user-emacs-directory "secrets/.authinfo.gpg"))
;; Auth Source debugging
;; (setq auth-source-debug t)

;;; [ Secrets ] -- presenting password entries retrieved by Security Service from freedesktop.org.

;; - Variable: `secrets-path'
;; - Command-Line Utility: `secret-tool'

;;; [ EasyPG Assistant ] --- transparent, automatic encryption and decryption.

;; (use-package pinentry
;;   :ensure t
;;   :ensure-system-package pinentry
;;   :config
;;   (pinentry-start))

(use-package epa
  :ensure t
  :load (epa-file)
  :init
  ;; force Emacs to use its own internal password prompt instead of an external
  ;; pin entry program.
  (setenv "GPG_AGENT_INFO" nil)
  ;; let EasyPG Assistant to use loopback for pinentry.
  (setq epa-pinentry-mode 'loopback)
  (epa-file-enable)
  :config
  (setq epa-file-encrypt-to "stardiviner")
  (setq epa-file-select-keys (case epa-file-encrypt-to
                               ('nil t)
                               (t nil)))
  ;; cache passphrase for symmetric encryption.
  ;; For security reasons, this option is turned off by default and
  ;; not recommended to use.  Instead, consider using gpg-agent which
  ;; does the same job in a safer way.
  (setq epa-file-cache-passphrase-for-symmetric-encryption t
        epa-file-inhibit-auto-save t)

  (add-to-list 'display-buffer-alist
               '("^\\*Keys\\*" (display-buffer-below-selected)))
  )

;;; [ inline-crypt ] -- simple inline encryption via openssl.

(use-package inline-crypt
  :ensure t
  :commands (inline-crypt-encrypt-region
             inline-crypt-decrypt-region
             inline-crypt-encrypt-string
             inline-crypt-decrypt-string)
  :preface
  (unless (boundp 'inline-crypt-prefix)
    (define-prefix-command 'inline-crypt-prefix))
  (define-key encrypt-prefix (kbd "i") 'inline-crypt-prefix)
  :bind (:map inline-crypt-prefix
              ("r" . inline-crypt-encrypt-region)
              ("C-r" . inline-crypt-decrypt-region)
              ("s" . inline-crypt-encrypt-string)
              ("C-s" . inline-crypt-decrypt-string))
  :config
  ;; (setq inline-crypt-openssl-command "openssl")
  )

;;; [ letterbox-mode ] -- a simple minor mode to add letterboxing to sensitive text.

(use-package letterbox-mode
  :ensure t
  :bind (:map encrypt-prefix
              ("l" . letterbox-toggle))
  )



(provide 'init-my-emacs-encrypt)

;;; init-my-emacs-encrypt.el ends here
