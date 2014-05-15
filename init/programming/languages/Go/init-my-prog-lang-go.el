;;; init-my-prog-lang-go.el --- init Go language Emacs settings

;;; Commentary:


;;; Code:

;;; [ go-mode ]

;;; Usage:
;; - `gofmt'
;; - `godoc'
;; - [C-c C-a] -- `go-import-add'
;; - `go-remove-unused-imports'
;; - `go-goto-imports'
;; - `go-play-buffer' and `go-play-region'
;; - `go-download-play'
;; - [C-c C-d] -- `godef-describe' and [C-c C-j] -- `godef-jump'
;; - `go-coverage'


(require 'go-mode)

;; godoc -- `go doc [QUERY]`
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-h d") 'godoc)))

;; gofmt
(add-hook 'before-save-hook #'gofmt-before-save)

;; use `godef-jump' instead of etags etc tags jumping.
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "M-.") #'godef-jump)))



;;; [ go-eldoc ]

(require 'go-eldoc) ; Don't need to require, if you install by package.el
(add-hook 'go-mode-hook 'go-eldoc-setup)


;;; [ gocode ]




;;; [ go-autocomplete ]


;;; [ go-company ]


;;; [ go-play ]



(provide 'init-my-prog-lang-go)

;;; init-my-prog-lang-go.el ends here
