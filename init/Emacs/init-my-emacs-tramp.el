;;; init-my-emacs-tramp.el --- init for tramp
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Tramp ]

;; Usage:
;; - [C-x C-f /sudo::/path/to/file]
;; - [C-x C-f /su::/path/to/file] -- edit stuff as root.
;; - [ssh]
;; - [telnet]

;; (require 'tramp)

;; (setq tramp-default-method "ssh"        ; default value is "scpc" (ssh and scp)
;;       ;; tramp-methods
;;       tramp-chunksize 500
;;       ;;; for debugging
;;       tramp-debug-buffer-name t
;;       tramp-verbose 10
;;       )

;; change SHELL environment variable to solve Tramp hangs issue.
;; (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))




(provide 'init-my-emacs-tramp)

;;; init-my-emacs-tramp.el ends here
