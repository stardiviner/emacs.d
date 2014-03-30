;;; [ Info ]
;; (setq Info-directory-list "/usr/share/info/"
;;       Info-additional-directory-list)

(setq Info-isearch-search t) ; let `s' search like =isearch= for incremental search.

;; (Info-mode-hook)



(setq suggest-key-bindings t) ; show the /equivalent/ key binding when [M-x] command has one.

(define-key global-map (kbd "C-h u") 'manual-entry) ; Unix

(provide 'init-my-emacs-help)
