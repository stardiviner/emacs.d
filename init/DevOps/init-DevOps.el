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
(define-key prog-tools-prefix (kbd "i") 'infrastructure-as-code-prefix)

;; (require 'init-puppet)
;; (require 'init-ansible)
;; (require 'init-terraform)

;;; [ Deployment ]

(unless (boundp 'deploy-prefix)
  (define-prefix-command 'deploy-prefix))
(define-key prog-tools-prefix (kbd "d") 'deploy-prefix)

;; (use-package copy-file-on-save
;;   :ensure t
;;   :defer t
;;   :init (global-copy-file-on-save-mode))

;;; [ heroku ]

(use-package heroku
  :ensure t
  :defer t
  :commands (heroku-run))


(provide 'init-DevOps)

;;; init-DevOps.el ends here
