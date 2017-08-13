;;; init-my-chemistry.el --- init for Chemistry.

;;; Commentary:



;;; Code:

;;; Chemistry: SMILES
(use-package smiles-mode
  :ensure t)
(use-package ob-smiles
  :ensure t
  :config
  (add-to-list 'org-babel-load-languages '(smiles . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  )




(provide 'init-my-chemistry)

;;; init-my-chemistry.el ends here
