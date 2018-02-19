;;; init-Graphviz.el --- init for Graphviz tools.

;;; Commentary:



;;; Code:

;;; [ graphviz-dot-mode ]

(use-package graphviz-dot-mode
  :ensure t
  :defer t
  :config
  (setq graphviz-dot-toggle-completions t
        graphviz-dot-auto-preview-on-save t)
  (define-key graphviz-dot-mode-map (kbd "TAB") 'graphviz-dot-complete-word)
  )


;;; [ ob-dot ]

(require 'ob-dot)

(add-to-list 'org-babel-load-languages '(dot . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot)) ; open dot src block with `graphviz-dot-mode'.
(add-to-list 'org-babel-tangle-lang-exts '("dot" . "dot"))



(provide 'init-Graphviz)

;;; init-Graphviz.el ends here
