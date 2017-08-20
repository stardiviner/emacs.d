;;; init-my-prog-devops.el --- init for DevOps
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(require 'init-container)
(require 'init-vagrant)
;; (require 'init-puppet)
;; (require 'init-ansible)

;;; [ Deployment ]

(use-package copy-file-on-save
  :ensure t
  :config
  (global-copy-file-on-save-mode)
  )


(provide 'init-my-prog-devops)

;;; init-my-prog-devops.el ends here
