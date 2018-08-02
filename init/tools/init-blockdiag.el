;;; init-blockdiag.el --- init for blockdiag tools.

;;; Commentary:



;;; Code:

;;; [ blockdiag ] -- Emacs interface to blockdiag.

(use-package ob-blockdiag
  :ensure t
  :ensure blockdiag-mode
  :defer t
  :init
  (add-to-list 'org-babel-load-languages '(blockdiag . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))



(provide 'init-blockdiag)

;;; init-blockdiag.el ends here
