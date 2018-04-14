;;; init-emacs-encrypt.el --- init Emacs encrypt & decrypt

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



(provide 'init-emacs-encrypt)

;;; init-emacs-encrypt.el ends here
