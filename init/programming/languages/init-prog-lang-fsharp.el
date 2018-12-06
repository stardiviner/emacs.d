;;; init-prog-lang-fsharp.el --- init for F#

;;; Commentary:



;;; Code:

;;; [ fsharp-mode ] --

(use-package fsharp-mode
  :ensure t)

;;; [ ob-fsharp ] -- Org-mode Babel support for F#.

(use-package ob-fsharp
  :ensure t
  :config
  (add-to-list 'org-babel-load-languages '(fsharp . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  )




(provide 'init-prog-lang-fsharp)

;;; init-prog-lang-fsharp.el ends here
