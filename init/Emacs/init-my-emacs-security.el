;;; init-my-emacs-security.el --- init for Emacs Security.

;;; Commentary:



;;; Code:

;;; [ File-local Variables ]

(setq enable-local-eval 'maybe
      enable-local-variables t ; :all
      ;; safe-local-eval-forms
      )

;;; [ certificate ]

;; (require 'tls)

;; (setq tls-checktrust 'ask)

;;; Fix SSL certificate issue on `gnutls'.
(setq tls-program
      '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof"
        "gnutls-cli --x509cafile %t -p %p %h"
        "gnutls-cli -p %p %h"))

;; (require 'gnutls)
;;
;; (setq gnutls-verify-error t
;;       ;; gnutls-trustfiles
;;       )


(provide 'init-my-emacs-security)

;;; init-my-emacs-security.el ends here
