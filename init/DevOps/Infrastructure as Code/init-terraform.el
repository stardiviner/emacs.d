;;; init-terraform.el --- init for Terraform

;;; Commentary:



;;; Code:

;;;  [ terraform-mode ]

(use-package terraform-mode
  :ensure t
  :ensure-system-package terraform
  :defer t
  :config
  (use-package company-terraform
    :ensure t
    :defer t
    :config
    (defun my-terraform-company-setup ()
      (my-company-add-backend-locally 'company-terraform))
    (add-hook 'terraform-mode-hook #'my-terraform-company-setup))
  )



(provide 'init-terraform)

;;; init-terraform.el ends here
