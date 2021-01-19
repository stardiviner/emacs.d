;;; init-emacs-encrypt.el --- init Emacs encrypt & decrypt

;;; Commentary:


;;; Code:

(unless (boundp 'tools-prefix)
  (define-prefix-command 'tools-prefix))
(unless (boundp 'encrypt-prefix)
  (define-prefix-command 'encrypt-prefix))
(define-key tools-prefix (kbd "M-e") 'encrypt-prefix)

;;; [ EasyPG Assistant ] --- transparent, automatic encryption and decryption.

(use-package pinentry
  :ensure t
  :hook (after-init . pinentry-start))

(use-package epa
  :ensure t
  ;; force Emacs to use its own internal password prompt instead of an external
  ;; pin entry program.
  :preface (setenv "GPG_AGENT_INFO" nil)
  :custom ((epa-pinentry-mode 'loopback) ; let EasyPG Assistant to use loopback for pinentry.
           ;; cache passphrase for symmetric encryption.
           ;; For security reasons, this option is turned off by default and
           ;; not recommended to use.  Instead, consider using gpg-agent which
           ;; does the same job in a safer way.
           (epa-file-cache-passphrase-for-symmetric-encryption t)
           (epa-file-inhibit-auto-save t))
  :init (epa-file-enable)
  (add-to-list 'display-buffer-alist '("^\\*Keys\\*" . (display-buffer-below-selected)))
  :config
  (setq epa-file-encrypt-to '("stardiviner" "numbchild@gmail.com"))
  (setq epa-file-select-keys (cl-case epa-file-encrypt-to ('nil t) (t nil)))
  ;; Decrypt to load session at Emacs startup beginning to avoid pause prompt.
  (my/json-read-value my/account-file 'ejc-sql-postgresql))



(provide 'init-emacs-encrypt)

;;; init-emacs-encrypt.el ends here
