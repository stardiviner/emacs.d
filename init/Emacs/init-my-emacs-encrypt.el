;;; init-my-emacs-encrypt.el --- init Emacs encrypt & decrypt

;;; Commentary:


;;; Code:

(unless (boundp 'my-tools-prefix)
  (define-prefix-command 'my-tools-prefix))
(unless (boundp 'my-encrypt-prefix)
  (define-prefix-command 'my-encrypt-prefix))
(define-key my-tools-prefix (kbd "e") 'my-encrypt-prefix)

;;; [ password ]

(setq password-cache-expiry nil) ; don't expire password cache.


;;; [ Auth Source ]

;; Auth Source debugging
(setq auth-source-debug t)


;;; [ EasyPG Assistant ] --- transparent, automatic encryption and decryption.

(use-package pinentry
  :config
  (pinentry-start)
  )

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
  
  (setq epa-file-encrypt-to "numbchild@gmail.com")
  (setq epa-file-select-keys (case epa-file-encrypt-to
                               ('nil t)
                               (t nil)))
  ;; cache passphrase for symmetric encryption.
  ;; For security reasons, this option is turned off by default and
  ;; not recommended to use.  Instead, consider using gpg-agent which
  ;; does the same job in a safer way.
  (setq epa-file-cache-passphrase-for-symmetric-encryption t
        epa-file-inhibit-auto-save t)
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

;;; [ letterbox-mode ] -- a simple minor mode to add letterboxing to sensitive text.

(use-package letterbox-mode
  :ensure t
  :bind (:map my-encrypt-prefix
              ("l" . letterbox-toggle))
  )



(provide 'init-my-emacs-encrypt)

;;; init-my-emacs-encrypt.el ends here
