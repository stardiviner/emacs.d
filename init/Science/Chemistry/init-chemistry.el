;;; init-chemistry.el --- init for Chemistry.

;;; Commentary:



;;; Code:

;;; Chemistry: SMILES
(use-package smiles-mode
  :ensure t
  :defer t
  :init
  (use-package ob-smiles
    :defer t
    :commands (org-babel-execute:smiles)
    :config
    (add-to-list 'org-babel-load-languages '(smiles . t))
    (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
    (add-to-list 'org-babel-tangle-lang-exts '("smiles" . "smiles"))))



(provide 'init-chemistry)

;;; init-chemistry.el ends here
