;;; init-ansible.el --- init for Ansible
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ ansible ] -- Ansible minor mode

(use-package ansible
  :ensure t
  :commands (ansible))

;;; [ ansible-doc ] -- Ansible documentation for GNU Emacs

(use-package ansible-doc ; [C-c ?] `ansible-doc'.
  :ensure t
  :defer t
  :init (add-hook 'yaml-mode-hook #'ansible-doc-mode))

;;; [ company-ansible ]

(use-package company-ansible
  :ensure t
  :defer t
  :init
  (defun my/company-ansible-setup ()
    (my-company-add-backend-locally 'company-ansible))
  (add-hook 'yaml-mode-hook #'my/company-ansible-setup))


(provide 'init-ansible)

;;; init-ansible.el ends here
