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
;;
;; (setq tls-checktrust 'ask
;;       ;; tls-program
;;       )

;; (require 'gnutls)
;;
;; (setq gnutls-verify-error t
;;       ;; gnutls-trustfiles
;;       )


(provide 'init-my-emacs-security)

;;; init-my-emacs-security.el ends here
