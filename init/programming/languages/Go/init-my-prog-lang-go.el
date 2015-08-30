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

(require 'go-mode-autoloads)

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

;; use `godef-jump' instead of etags etc tags jumping.
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "M-.") #'godef-jump)))




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

;; (require 'go-eldoc) ; Don't need to require, if you install by package.el
;; (add-hook 'go-mode-hook 'go-eldoc-setup)


;;; [ gocode ] -- An autocompletion daemon for the Go programming language.


;;; [ go-complete ] -- Native Go completion for Emacs.

;; A `gocode' based native completion system for Emacs, can be integrated with
;; frameworks like ido, company, autocomplete.

(add-hook 'completion-at-point-functions 'go-complete-at-point)


;;; [ go-autocomplete ]


;;; [ go-company ]


;;; [ company-go ]

(if (getenv "GOPATH")
    (progn
      (load (concat (getenv "GOPATH") "/src/github.com/nsf/gocode/emacs-company/company-go.el"))
      (require 'company-go))
  (error "SHELL env $GOPATH not available, set it in your SHELL"))

(add-hook 'go-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends) (add-to-list 'company-backends 'company-go))
            (company-mode t)))

(eval-after-load 'company-go
  '(setq company-go-show-annotation t
         company-go-begin-after-member-access t
         company-go-insert-arguments t
         ;; company-go-gocode-command "gocode"
         ))


;;; [ go-play ]



(provide 'init-my-prog-lang-go)

;;; init-my-prog-lang-go.el ends here
