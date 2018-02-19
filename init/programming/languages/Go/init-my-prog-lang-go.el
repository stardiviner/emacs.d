;;; init-my-prog-lang-go.el --- init Go language Emacs settings

;;; Commentary:


;;; Code:

;;; [ go-mode ]

(use-package go-mode
  :ensure t
  :ensure-system-package go
  :defer t
  :config
  (setq gofmt-command "gofmt"
        godef-command "godef"
        godoc-command "go doc"
        godoc-and-godef-command "godoc"
        godoc-use-completing-read t
        godoc-at-point-function #'godoc-and-godef ; `godoc-and-godef', `godoc-gogetdoc'
        )
  
  (defun my-go-mode-settings ()
    ;; go-import [C-u] + [C-c C-a]
    ;; (local-set-key (kbd "C-c C-S-a") 'go-remove-unused-imports)
    ;; gofmt
    (local-set-key (kbd "C-c C-f") 'gofmt)
    ;; godoc -- `go doc [QUERY]`
    (local-set-key (kbd "C-h d d") 'godoc-at-point)
    ;; (local-set-key (kbd "C-c C-k") 'godoc)
    ;; godef
    ;; use `godef-jump' instead of `etags' etc tags jumping.
    (local-set-key (kbd "M-.") #'godef-jump)
    )
  
  (add-hook 'go-mode-hook #'my-go-mode-settings)
  
  (add-hook 'before-save-hook #'gofmt-before-save)
  )

;; [ ob-go ]

(use-package ob-go
  :ensure t
  :defer t
  :init
  (add-to-list 'org-babel-load-languages '(go . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("go" . "go"))
  )



;;; [ go-gopath ] -- guess GOPATH using gb and projectile.

(use-package go-gopath
  :ensure t
  :defer t
  :after go-mode
  :init
  (define-key go-mode-map (kbd "C-x C-e") #'go-gopath-set-gopath)
  )


;;; [ go-eldoc ]

(use-package go-eldoc
  :ensure t
  :defer t
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  )


;;; [ gocode ] -- An autocompletion daemon for the Go programming language.

;; [ company-go ] -- company-mode backend for Go (using gocode).

(use-package company-go
  :ensure t
  ;; :load-path (lambda () (concat (getenv "GOPATH") "/src/github.com/nsf/gocode/emacs-company/company-go.el"))
  :ensure-system-package ((gocode . "go get -u github.com/nsf/gocode")
                          (liteide . "sudo pacman -S --noconfirm liteide"))
  :config
  (setq company-go-show-annotation t
        company-go-begin-after-member-access t
        company-go-insert-arguments t
        ;; company-go-gocode-args
        )

  (defun my-company-go-setup ()
    (my-company-add-backend-locally 'company-go)
    (setq-local company-minimum-prefix-length 2))
  (add-hook 'go-mode-hook #'my-company-go-setup)
  )


;;; [ gorepl-mode ] -- Go REPL Interactive Development in top of Gore.

;; (use-package gorepl-mode
;;   :ensure t
;;   :bind (:map go-mode-map
;;               ("C-c C-s" . gorepl-run)
;;               ("C-c C-z" . gorepl-run)
;;               ("C-c C-l" . gorepl-run-load-current-file)
;;               ("C-c C-r" . gorepl-eval-region)
;;               ("C-c C-e" . gorepl-eval-line)
;;               )
;;   :init
;;   ;; default setup mapping (this will override `go-goto-map')
;;   ;; (add-hook 'go-mode-hook #'gorepl-mode)
;;   )


;;; [ go-errcheck ] -- errcheck integration for go-mode.

;; (use-package go-errcheck
;;   :ensure t)

;;; [ go-oracle ]

;; (use-package go-oracle
;;   :load-path "$GOPATH/src/golang.org/x/tools/cmd/oracle/"
;;   :config
;;   (require 'go-oracle))

;;; [ go-guru ] -- Integration of the Go 'guru' analysis tool into Emacs.

;; (use-package go-guru
;;   :ensure t
;;   :config
;;   (setq go-guru-debug t)
;;   )

;;; [ gotest ] -- Launch GO unit tests


;;; [ govet ] -- linter/problem finder for the Go source code.


;;; [ go-projectile ] -- Projectile GOPATH.

;; (use-package go-projectile
;;   :ensure t)



(provide 'init-my-prog-lang-go)

;;; init-my-prog-lang-go.el ends here
