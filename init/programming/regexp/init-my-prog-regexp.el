;;; init-prog-regexp.el --- Summary
;;
;;; Commentary:


;;; Code:

;;; [ re-builder ]
(require 're-builder)

;;; [ re-builder+ ]
;; (unless (package-installed-p 're-builder+)
;;   (package-install 're-builder+))
;; (require 're-builder+)
;; TODO what's the difference for 'read, 'string and 'rx.
(setq reb-re-syntax 'read) ; 'read, 'string, 'rx

;;; [ rx ]

;;; regex-tool
;; https://github.com/jwiegley/regex-tool




(provide 'init-my-prog-regexp)

;;; init-my-prog-regexp.el ends here
