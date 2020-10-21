;;; init-prog-lang-julia.el --- init for Julia
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ julia-mode ]

(use-package julia-mode
  :ensure t
  :defer t
  :commands (run-julia inferior-julia)
  :config (setq julia-indent-offset 2))

;;; [ ESS - Julia ]

(use-package ess
  :ensure t
  :defer t
  :commands (julia)
  :init
  ;; add `julia-mode' to `prog-mode''.
  (add-hook 'julia-mode-hook
            (lambda () (unless (derived-mode-p 'prog-mode) (run-hooks 'prog-mode-hook)))))

;;; [ ob-julia ]

(use-package ess
  :ensure t
  :after org
  :preface (define-obsolete-variable-alias 'inferior-julia-program-name 'inferior-julia-program)
  :custom (inferior-julia-program (or (executable-find "julia-basic") "julia"))
  :init
  (use-package ob-julia
    :defer t
    :custom (org-babel-julia-command "julia")
    :commands (org-babel-execute:julia)
    :config
    (add-to-list 'org-babel-load-languages '(julia . t))
    (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)))

;;; [ julia-shell ] -- Emacs major mode for an interactive Julia shell.

;; (use-package julia-shell
;;   :ensure t
;;   :commands (run-julia))

;;; [ flycheck-julia ] -- Add a julia syntax checker to flycheck using Lint.jl

;; (use-package flycheck-julia
;;   :ensure t
;;   :defer t
;;   :config
;;   (flycheck-julia-setup)
;;   ;; if you use `flycheck-global-mode'
;;   ;; (add-to-list 'flycheck-global-modes 'julia-mode)
;;   ;; (add-to-list 'flycheck-global-modes 'ess-julia-mode)
;;   )


(provide 'init-prog-lang-julia)

;;; init-prog-lang-julia.el ends here
