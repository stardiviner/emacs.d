;;; init-my-emacs-help.el --- init Emacs's help settings.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Info ]
;; (setq Info-directory-list "/usr/share/info/"
;;       Info-additional-directory-list)

(setq Info-isearch-search t) ; let `s' search like =isearch= for incremental search.

;; (Info-mode-hook)



(setq suggest-key-bindings t) ; show the /equivalent/ key binding when [M-x] command has one.

(define-key global-map (kbd "C-h u") 'manual-entry) ; Unix


;;; [ Discover.el ]

;;; https://github.com/mickeynp/discover.el

;;  Useful Helper Commands
;;
;; You can get the name of the command that reveals a given context menu by
;; calling `discover-get-context-menu-command-name'. If you just want to funcall
;; the returned symbol, the function `discover-show-context-menu' will do this
;; for you.

;; (require 'discover)
;; (global-discover-mode 1)

;; (when (featurep 'discover)
;;   (discover-add-context-menu))




(provide 'init-my-emacs-help)

;;; init-my-emacs-help.el ends here
