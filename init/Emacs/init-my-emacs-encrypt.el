;;; init-my-emacs-encrypt.el --- init Emacs encrypt & decrypt

;;; Commentary:


;;; Code:

(unless (boundp 'tools-prefix)
  (define-prefix-command 'tools-prefix))
(unless (boundp 'encrypt-prefix)
  (define-prefix-command 'encrypt-prefix))
(define-key tools-prefix (kbd "e") 'encrypt-prefix)

;;; [ password ]

(setq password-cache-expiry nil) ; don't expire password cache.


;;; [ Auth-Source ]

(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))
;; Auth Source debugging
(setq auth-source-debug t)


;;; [ EasyPG Assistant ] --- transparent, automatic encryption and decryption.

;; (use-package pinentry
;;   :ensure t
;;   :ensure-system-package pinentry
;;   :config
;;   (pinentry-start))

(use-package epa
  :ensure t
  :init
  (require 'epa-file)
  ;; force Emacs to use its own internal password prompt instead of an external
  ;; pin entry program.
  (setenv "GPG_AGENT_INFO" nil)
  (epa-file-enable)
  :config
  ;; let EasyPG Assistant to use loopback for pinentry.
  (setq epa-pinentry-mode 'loopback)
  
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

;;; FAQ
;; How to use a non-graphical password prompt for gpg?
;;
;; In X, Emacs 23 seems to pop up a graphical window to ask you the
;; keyphrase. How to let it ask the password in minibuffer?
;;
;; I found the cause for it. The graphical window appears if you run gpg with
;; the --use-agent option. EasyPG adds it (see epg.el) if it sees an environment
;; variable like this one:
;; GPG_AGENT_INFO=/tmp/seahorse-nDQm50/S.gpg-agent:6321:1 (check that with the
;; env command). And you have this variable if for instance you have the program
;; seahorse installed and running (which is the case in Ubuntu). If you
;; uninstall Seahorse, the prompt will always be text instead of graphical. You
;; may have to relogin to X to force Seahorse to close.

;;; [ inline-crypt ] -- simple inline encryption via openssl.

(use-package inline-crypt
  :ensure t
  :commands (inline-crypt-encrypt-region
             inline-crypt-decrypt-region
             inline-crypt-encrypt-string
             inline-crypt-decrypt-string)
  :config
  ;; (setq inline-crypt-openssl-command "openssl")

  (unless (boundp 'inline-crypt)
    (define-prefix-command 'inline-crypt-prefix))
  (define-key encrypt-prefix (kbd "i") 'inline-crypt-prefix)

  (define-key inline-crypt-prefix (kbd "r") 'inline-crypt-encrypt-region)
  (define-key inline-crypt-prefix (kbd "C-r") 'inline-crypt-decrypt-region)
  (define-key inline-crypt-prefix (kbd "s") 'inline-crypt-encrypt-string)
  (define-key inline-crypt-prefix (kbd "C-s") 'inline-crypt-decrypt-string)
  )

;;; [ letterbox-mode ] -- a simple minor mode to add letterboxing to sensitive text.

(use-package letterbox-mode
  :ensure t
  :bind (:map encrypt-prefix
              ("l" . letterbox-toggle))
  )



(provide 'init-my-emacs-encrypt)

;;; init-my-emacs-encrypt.el ends here
