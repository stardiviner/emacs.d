;;; init-chef.el --- init for chef.

;;; Commentary:



;;; Code:

;;; [ org-chef ] -- A package for making a cookbook and managing recipes with Org-mode.

(use-package org-chef
  :ensure t
  :defer t
  :config
  (add-to-list 'org-capture-templates
               '("c" "Cookbook" entry
                 (file (concat org-directory "/Cookbook.org"))
                 "%(org-chef-get-recipe-from-url)"
                 :empty-lines 1))
  )



(provide 'init-chef)

;;; init-chef.el ends here
