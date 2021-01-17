;;; init-AWS.el --- init file for Amazon AWS

;;; Commentary:



;;; Code:

;;; [ awscli-capf ] -- Completion at point function for AWS CLI commands and parameters.

(use-package awscli-capf
  :ensure t
  :commands (awscli-capf-add)
  :hook (shell-mode . awscli-capf-add))

;;; [ helm-aws ] -- 

;;; [ aws-ec2 ] --

;;; [ aws-snippets ] --



(provide 'init-AWS)

;;; init-AWS.el ends here
