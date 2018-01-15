;;; init-DevOps.el --- init for DevOps
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Virtualization ]

(require 'init-container)
(require 'init-vagrant)

;;; [ Configuration Manager (CMs) ]

(unless (boundp 'config-manager-prefix)
  (define-prefix-command 'config-manager-prefix))
(define-key prog-tools-prefix (kbd "c") 'config-manager-prefix)

;; (require 'init-puppet)
;; (require 'init-ansible)

;;; [ Deployment ]

(unless (boundp 'deploy-prefix)
  (define-prefix-command 'deploy-prefix))
(define-key prog-tools-prefix (kbd "c") 'deploy-prefix)

(use-package copy-file-on-save
  :ensure t
  :config
  (global-copy-file-on-save-mode)
  )

;;; [ heroku ]

(use-package heroku
  :ensure t
  :defer t)


(provide 'init-DevOps)

;;; init-DevOps.el ends here
