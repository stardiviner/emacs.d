;;; init-ansible.el --- init for Ansible
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ ansible ] -- Ansible minor mode

(use-package ansible
  :ensure t)


;;; [ ansible-doc ] -- Ansible documentation for GNU Emacs

(use-package ansible-doc
  :ensure t
  :config
  (add-hook 'yaml-mode-hook #'ansible-doc-mode) ; [C-c ?] `ansible-doc'.
  )


;;; [ company-ansible ]

(use-package company-ansible
  :ensure t
  :config
  ;; (add-to-list 'company-backends 'company-ansible)
  )


(provide 'init-ansible)

;;; init-ansible.el ends here
