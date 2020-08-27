;;; init-prog-lang-go.el --- init Go language Emacs settings

;;; Commentary:


;;; Code:

;;; [ go-mode ]

(use-package go-mode
  :ensure t
  :defer t
  :custom (godoc-use-completing-read t)
  :init (add-to-list 'display-buffer-alist '("^\\*godoc .*\\*" . (display-buffer-below-selected)))
  :hook (before-save . gofmt-before-save)
  :config
  (defun my-go-mode-settings ()
    ;; go-import [C-c C-a]
    ;; gofmt
    (local-set-key (kbd "C-c C-f") 'gofmt)
    ;; godoc -- `go doc [QUERY]`
    (local-set-key (kbd "C-h d d") 'godoc-at-point)
    ;; (local-set-key (kbd "C-c C-k") 'godoc)
    ;; godef
    ;; [C-c C-d] `godef-describe'
    ;; use `godef-jump' instead of `etags' etc tags jumping.
    (local-set-key (kbd "M-.") #'godef-jump))
  (add-hook 'go-mode-hook #'my-go-mode-settings))

;; [ ob-go ]

(use-package ob-go
  :ensure t
  :defer t
  :commands (org-babel-execute:go)
  :config
  (add-to-list 'org-babel-load-languages '(go . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("go" . "go")))

;;; [ go-projectile ] -- Projectile GOPATH.

;; (use-package go-projectile
;;   :ensure t
;;   :defer t
;;   :commands (go-projectile-install-tools))

;;; [ go-gopath ] -- guess GOPATH using gb and projectile.

(use-package go-gopath
  :ensure t
  :after go-mode
  :defer t
  :bind (:map go-mode-map ("C-x C-e" . go-gopath-set-gopath)))

;;; [ go-eldoc ]

(use-package go-eldoc
  :ensure t
  :defer t
  :init (add-hook 'go-mode-hook 'go-eldoc-setup))

;;; [ lsp-go ] -- Go support for lsp-mode using Sourcegraph's Go Language Server.

(use-package lsp-mode
  :ensure t
  :defer t
  :hook (go-mode . lsp))

;;; [ gorepl-mode ] -- Go REPL Interactive Development in top of Gore.

(use-package gorepl-mode
  :ensure t
  :defer t
  :commands (gorepl-run)
  ;; default setup mapping (this will override `go-goto-map')
  :hook (go-mode-hook . gorepl-mode))

;;; [ go-errcheck ] -- errcheck integration for go-mode.

;; (use-package go-errcheck
;;   :ensure t)

;;; [ go-guru ] -- Integration of the Go 'guru' analysis tool into Emacs.

(use-package go-guru
  :ensure t
  :defer t
  :init (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode))

;;; [ go-imports ] -- Insert go import statement given package name.

(use-package go-imports
  :ensure t
  :defer t
  :commands (go-imports-insert-import go-imports-reload-packages-list)
  :bind (:map go-mode-map ("C-c I" . go-imports-insert-import)))

;;; [ gotest ] -- Emacs mode to go unit test command line tool.

(use-package gotest
  :ensure t
  :defer t
  :commands (go-run
             go-test-current-test go-test-current-file go-test-current-project
             go-test-current-benchmark))



(provide 'init-prog-lang-go)

;;; init-prog-lang-go.el ends here
