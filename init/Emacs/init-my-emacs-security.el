;;; init-my-emacs-security.el --- init for Emacs Security.

;;; Commentary:



;;; Code:

;;; File Local Variables



;;; [ certificate ]

(require 'tls)

(setq tls-checktrust 'ask
      ;; tls-program
      )

(require 'gnutls)

(setq gnutls-verify-error t
      ;; gnutls-trustfiles
      )


(provide 'init-my-emacs-security)

;;; init-my-emacs-security.el ends here
