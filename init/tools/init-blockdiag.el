;;; init-blockdiag.el --- init for blockdiag tools.

;;; Commentary:



;;; Code:

;;; [ blockdiag ] -- Emacs interface to blockdiag.

(use-package ob-blockdiag
  :ensure t
  :defer t
  :config
  (add-to-list 'org-babel-load-languages '(blockdiag . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  
  (use-package blockdiag-mode
    :ensure t)
  )



(provide 'init-blockdiag)

;;; init-blockdiag.el ends here
