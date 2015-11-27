;;; init-my-prog-lang-go.el --- init Go language Emacs settings

;;; Commentary:


;;; Code:

;;; [ go-mode ]

(use-package go-mode
  :config
  ;; (setq go-command "go"
  ;;       gofmt-command "gofmt"
  ;;       gofmt-show-errors 'buffer         ; 'buffer, 'echo, nil
  ;;       godef-command "godef"
  ;;       )

  ;; godoc -- `go doc [QUERY]`
  (add-hook 'go-mode-hook
            (lambda ()
              (local-set-key (kbd "C-h d d") 'godoc)))

  ;; gofmt
  (add-hook 'before-save-hook #'gofmt-before-save)

  ;; godef
  ;; use `godef-jump' instead of `etags' etc tags jumping.
  (add-hook 'go-mode-hook (lambda ()
                            (local-set-key (kbd "M-.") #'godef-jump)))
  )


;;; customization

;; (defun go-run-buffer()
;;   (interactive)
;;   (shell-command (concat "go run " (buffer-name))))
;; (define-key go-mode-map (kbd "C-c C-c") 'go-run-buffer)

;; (defun go-kill()
;;   (interactive)
;;   (if (go-mode-in-string)
;;       (paredit-kill-line-in-string)
;;     (paredit-kill)))
;; (defun go-backward-delete()
;;   (interactive)
;;   (if (go-mode-in-string)
;;       (paredit-backward-delete-in-string)
;;     (paredit-backward-delete)))

;; (add-hook 'go-mode-hook
;;           (lambda ()
;;             (auto-complete-mode 1)
;;             (add-to-list 'ac-sources 'ac-source-go)
;;             (call-process "gocode" nil nil nil "-s")))



;;; [ go-eldoc ]

(use-package go-eldoc
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))


;;; [ gocode ] -- An autocompletion daemon for the Go programming language.


;;; [ go-complete ] -- Native Go completion for Emacs.

;; A `gocode' based native completion system for Emacs, can be integrated with
;; frameworks like ido, company, autocomplete.

(add-hook 'go-mode-hook
          '(lambda ()
             (add-hook (make-local-variable 'completion-at-point-functions)
                       'go-complete-at-point)))


;;; [ go-autocomplete ]


;;; [ go-company ]


;;; [ company-go ]

(use-package company-go
  :config
  (if (getenv "GOPATH")
      (progn
        ;; TODO: do I need to use liteide's `gocode' instead?
        (load (concat (getenv "GOPATH") "/src/github.com/nsf/gocode/emacs-company/company-go.el"))
        )
    (error "SHELL env $GOPATH not available, set it in your SHELL"))

  (setq company-go-show-annotation t
        company-go-begin-after-member-access t
        company-go-insert-arguments t
        )
  
  (add-hook 'go-mode-hook
            '(lambda ()
               (add-to-list (make-local-variable 'company-backends) 'company-go)))
  )


;;; [ gorepl-mode ] -- A minor emacs mode for Go REPL

(use-package gorepl-mode
  :config
  (add-hook 'go-mode-hook #'gorepl-mode))


;;; [ go-play ] -- Paste to play.golang.org

;;; Usage:
;;
;; - [M-x go-play-buffer]
;; - [M-x go-play-region]


;;; [ go-playground ]

;;; Usage:
;;
;; - [M-x go-playground] :: start playground buffer for new snippet.
;; - [C-RET] :: compile and run your code.
;; - [M-x go-playground-remove-current-snippet] :: removing snippet after you done with it.

(use-package go-playground
  :config
  (setq go-playground-basedir "~/.go/src/playground")
  )


;;; [ gore-mode ] -- Simple mode for gore, a command-line evaluator for golang.


;;; [ flycheck ]

;; use linter: `go-errcheck'.


;;; [ go-errcheck ]

(use-package go-errcheck
  :config
  )


;;; [ gotest ] -- Launch GO unit tests


;;; [ govet ] -- linter/problem finder for the Go source code.


;;; [ go-projectile ] -- Projectile GOPATH.

(use-package go-projectile
  )


(provide 'init-my-prog-lang-go)

;;; init-my-prog-lang-go.el ends here
