;;; init-tramp.el --- init for tramp
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Tramp ]

(use-package tramp
  :ensure t
  :config
  ;; hotfix
  ;; (setq tramp-ssh-controlmaster-options
  ;;       "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
  ;; (setq tramp-ssh-controlmaster-options
  ;;       "-o ControlMaster=auto -o ControlPath='ssh_%C' -o ControlPersist=no")

  
  ;; <default method>
  (setq tramp-default-method "ssh") ; default "scp" (ssh + scp),
  ;; (add-to-list 'tramp-default-method-alist '("" ""))

  ;; speed-up tramp.
  (setq tramp-completion-reread-directory-timeout nil)

  ;; Multi-hops SSH proxy bridges
  ;; [[info:tramp#Multi-hops][info:tramp#Multi-hops]]
  ;;
  ;; (add-to-list 'tramp-default-proxies-alist '(HOST USER SSH-PROXY))

  ;; ad-hoc proxy
  ;; (setq tramp-save-ad-hoc-proxies t)
  ;;
  ;; <default user>
  ;; ssh connect: host, user
  ;; NOTE: this cause `ob-shell' :dir /sudo:: error.
  ;; (add-to-list 'tramp-default-user-alist
  ;;              '(("\\`su\\(do\\)?\\'" nil "root")))
  ;; (add-to-list 'tramp-default-user-alist
  ;;              '("ssh" ".*\\.somewhere\\.else\\'" "john"))
  ;;
  ;; <default host>
  ;; (add-to-list 'tramp-default-host-alist)
  ;;
  ;; <default proxy>
  ;; (add-to-list 'tramp-default-proxies-alist)
  ;;
  ;; Tramp completion
  ;; (add-to-list 'tramp-completion-function-alist)
  ;;
  ;; <predefined connection property>
  ;; (add-to-list 'tramp-connection-properties
  ;;              (list (regexp-quote "/ssh:user@randomhost.your.domain:")
  ;;                    "busybox" t))

  (setq tramp-auto-save-directory "/tmp")

  ;; tramp-debug-buffer-name t
  ;; (setq tramp-verbose 10)


  ;; change SHELL environment variable to solve Tramp hangs issue.
  ;; (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

  ;; [ sh ]
  (require 'tramp-sh)
  (add-to-list 'tramp-remote-path "~/bin")
  
  ;; [ sudo in Tramp ]

  ;; [ Android adb ]
  ;; (setq tramp-adb-program "adb")
  )

;;; [ counsel-tramp ] -- Tramp with Ivy/counsel interface.

(use-package counsel-tramp
  :ensure t
  :defer t
  :commands (counsel-tramp)
  :config
  (defalias 'exit-tramp 'tramp-cleanup-all-buffers)
  )

;;; [ helm-tramp ] -- Tramp with Helm interface.

;; (use-package helm-tramp
;;   :ensure t
;;   :config
;;   (setq tramp-default-method "ssh")
;;   (defalias 'exit-tramp 'tramp-cleanup-all-buffers)
;;   (with-eval-after-load 'helm-config
;;     (define-key helm-command-map (kbd "M-t") 'helm-tramp))
;;   )



(provide 'init-tramp)

;;; init-tramp.el ends here
