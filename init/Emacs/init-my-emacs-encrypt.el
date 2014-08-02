;;; init-my-emacs-encrypt.el --- init Emacs encrypt & decrypt

;;; Commentary:


;;; Code:

;;; [ GnuPG ]


;;; [ EasyPG ] --- setup for transparent, automatic encryption and decryption.
(require 'epa)

(require 'epa-file)
(epa-file-enable)

(setq epa-file-encrypt-to "numbchild@gmail.com"
      ;; epa-gpg-program "/usr/bin/gpg"
      epa-file-select-keys t ; use public key
      epa-file-cache-passphrase-for-symmetric-encryption nil
      epa-file-inhibit-auto-save t)



(provide 'init-my-emacs-encrypt)

;;; init-my-emacs-encrypt.el ends here
