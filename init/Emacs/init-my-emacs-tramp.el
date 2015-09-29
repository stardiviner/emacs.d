;;; init-my-emacs-tramp.el --- init for tramp
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Tramp ]

(require 'tramp)

;; <default method>
;; (setq tramp-default-method "ssh") ; default "scpc" (ssh + scp),
;; (add-to-list 'tramp-default-method-alist '("" ""))

;; <default user>
;; ssh connect: host, user
;; (add-to-list 'tramp-default-user-alist
;;              '("ssh" ".*\\.somewhere\\.else\\'" "john"))

;; <default host>
;; (add-to-list 'tramp-default-host-alist)

;; <default proxy>
;; (add-to-list 'tramp-default-proxies-alist)

;; Tramp completion
;; (add-to-list 'tramp-completion-function-alist)

;; <predefined connection property>
;; (add-to-list 'tramp-connection-properties
;;              (list (regexp-quote "/ssh:user@randomhost.your.domain:")
;;                    "busybox" t))

;; tramp-debug-buffer-name t
;; tramp-verbose 10


;; change SHELL environment variable to solve Tramp hangs issue.
;; (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))




(provide 'init-my-emacs-tramp)

;;; init-my-emacs-tramp.el ends here
