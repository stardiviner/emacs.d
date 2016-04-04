;;; init-my-prog-lang-R.el --- init R

;;; Commentary:


;;; Code:

;;; [ R-mode ]

;;; Usage:
;;
;; - [M-x R] :: start R process.
;; - 'ess-R-describe-object-at-point-commands' / [C-c C-d C-e] ::
;; - [C-c C-e C-t] runs the command `ess-build-tags-for-directory'

(autoload 'R-mode "ess-site" "R-mode" t)

;; automatically get the correct mode
(add-to-list 'auto-mode-alist '("\\.[rR]\\'" . R-mode))
(add-to-list 'auto-mode-alist '("\\.Rd\\'" . Rd-mode)) ; R documentation
(add-to-list 'auto-mode-alist '("\\.S\\'" . S-mode))

(add-to-list 'auto-mode-alist '("\\.Rprofile\\'" . R-mode))
(add-to-list 'auto-mode-alist '("\\.Renviron\\'" . R-mode))

;; comment out the following if you are not using R/S-Plus on ACPUB
;; add a ";" in front of each line
;; (load "/usr/pkg/ess/lisp/ess-site")
;; (setq-default inferior-S+6-program-name "Splus")

;; with --no-readline argument.
(setq ess-R-readline nil)

;;; set prompt
;; (setq inferior-ess-primary-prompt "ℝ> ")
;;; handle the custom ℝ prompt in ess. Don’t use custom here.
;; (setq inferior-S-prompt "[]a-zA-Z0-9.[]*\\(?:[>+.] \\)*ℝ+> ")


;; start the ESS process if not started when open ESS buffers.
;; (add-hook 'after-init-hook 'R)

;; (define-key my-inferior-ess-map (kbd "R") 'R)

(define-key my-inferior-ess-map (kbd "R")
  '(lambda ()
     (my-func/open-and-switch-to-buffer 'R "*R*" t)))


;;; completing support

;; - `ess-company-backends' :: for company-mode.
;; - `ess-ac-sources' :: for auto-complete.


;;; temporally fix lintr missing for flycheck issue.

(add-hook 'R-mode-hook
          (lambda ()
            (flycheck-mode -1)))



(provide 'init-my-prog-lang-R)

;;; init-my-prog-lang-R.el ends here
