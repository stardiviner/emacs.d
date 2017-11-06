;;; init-ansible.el --- init for Ansible
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ ansible ] -- Ansible minor mode

(use-package ansible
  :ensure t
  :defer t)


;;; [ ansible-doc ] -- Ansible documentation for GNU Emacs

(use-package ansible-doc
  :ensure t
  :defer t
  :init
  (add-hook 'yaml-mode-hook #'ansible-doc-mode) ; [C-c ?] `ansible-doc'.
  )


;;; [ company-ansible ]

(use-package company-ansible
  :ensure t
  :defer t
  :init
  (add-hook 'yaml-mode-hook
            (lambda ()
              (my-company-add-backend-locally 'company-ansible)
              ))
  )


(provide 'init-ansible)

;;; init-ansible.el ends here
