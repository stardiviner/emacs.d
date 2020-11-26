;;; init-chef.el --- init for chef.

;;; Commentary:



;;; Code:

(with-eval-after-load 'org-capture
  (add-to-list 'org-capture-templates
               `("F" ,(format "%s\tChef Recipe Cookbook"
                              (all-the-icons-material "restaurant" :face 'all-the-icons-maroon))
                 entry
                 (file (concat org-directory
                               "/Wiki/Chef/Recipes/Data/Manuals/My Recipes Reference/My Recipes Reference.org"))
                 "* %^{Recipe Name}"
                 :empty-lines 1)
               :append))


;;; [ org-chef ] -- A package for making a cookbook and managing recipes with Org-mode.

;; (use-package org-chef
;;   :ensure t
;;   :defer t
;;   :init
;;   (with-eval-after-load 'org-capture
;;     (add-to-list 'org-capture-templates
;;                  `("F" ,(format "%s\tChef Recipe Cookbook"
;;                                 (all-the-icons-faicon "beer" :face 'all-the-icons-maroon))
;;                    entry
;;                    (file "~/Org/Wiki/Chef/Recipes/Data/Manuals/My Recipes Reference/My Recipes Reference.org")
;;                    "%(org-chef-get-recipe-from-url)"
;;                    :empty-lines 1)
;;                  :append)))



(provide 'init-chef)

;;; init-chef.el ends here
