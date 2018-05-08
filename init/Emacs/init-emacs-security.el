;;; init-emacs-security.el --- init for Emacs Security.

;;; Commentary:



;;; Code:

;;; [ File-local Variables ]

(setq enable-local-eval 'maybe
      enable-local-variables t ; :all
      ;; safe-local-eval-forms
      )

;;; [ password ] -- authentication sources for Gnus and Emacs.

(setq password-cache-expiry nil) ; don't expire password cache.

;;; [ auth-source ]

(add-to-list 'auth-sources (concat user-emacs-directory "secrets/.authinfo.gpg"))
;; Auth Source debugging
;; (setq auth-source-debug t)

(defun my:auth-source-get (query-key query-value get-key)
  "Get :secret of QUERY matched auth-source entries.
Usage: (my:auth-source-get :host \"api.heroku.com\" :user)"
  (pcase get-key
    (:secret
     (car (aref (aref (plist-get (car (auth-source-search query-key query-value)) :secret) 2) 0)))
    (:user
     (plist-get (car (auth-source-search query-key query-value)) :user))
    (:host
     (plist-get (car (auth-source-search query-key query-value)) :host))))

;;; [ Secrets ] -- presenting password entries retrieved by Security Service from freedesktop.org.

;; - Variable: `secrets-path'
;; - Command-Line Utility: `secret-tool'

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


(provide 'init-emacs-security)

;;; init-emacs-security.el ends here
