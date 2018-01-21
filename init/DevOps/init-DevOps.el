;;; init-DevOps.el --- init for DevOps
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Virtualization ]

(require 'init-container)
(require 'init-vagrant)

;;; [ Infrastructure as Code ]

(unless (boundp 'infrastructure-as-code-prefix)
  (define-prefix-command 'infrastructure-as-code-prefix))
(define-key prog-tools-prefix (kbd "c") 'infrastructure-as-code-prefix)

;; (require 'init-puppet)
;; (require 'init-ansible)
(require 'init-terraform)

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
