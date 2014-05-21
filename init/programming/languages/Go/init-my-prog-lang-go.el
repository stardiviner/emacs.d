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




;;; customizations
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

(require 'go-eldoc) ; Don't need to require, if you install by package.el
(add-hook 'go-mode-hook 'go-eldoc-setup)


;;; [ gocode ]




;;; [ go-autocomplete ]


;;; [ go-company ]


;;; [ go-play ]



(provide 'init-my-prog-lang-go)

;;; init-my-prog-lang-go.el ends here
