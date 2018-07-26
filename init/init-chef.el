;;; init-chef.el --- init for chef.

;;; Commentary:



;;; Code:

(add-to-list 'org-capture-templates
             '("F" "Chef [F]ood Cookbook" entry
               (file "~/Org/Wiki/Chef/Recipes/Data/Manuals/My Recipes Reference/My Recipes Reference.org")
               "* %^{Recipe Name}"
               :empty-lines 1))


;;; [ org-chef ] -- A package for making a cookbook and managing recipes with Org-mode.

;; (use-package org-chef
;;   :ensure t
;;   :config
;;   (add-to-list 'org-capture-templates
;;                '("F" "Chef [F]ood Cookbook" entry
;;                  (file "~/Org/Wiki/Chef/Recipes/Data/Manuals/My Recipes Reference/My Recipes Reference.org")
;;                  "%(org-chef-get-recipe-from-url)"
;;                  :empty-lines 1))
;;   )



(provide 'init-chef)

;;; init-chef.el ends here
