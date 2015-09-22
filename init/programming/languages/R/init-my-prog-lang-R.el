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

(setq-default inferior-R-program-name "R" ; /path/to/R
              ;; inferior-R-args ""
              ;; inferior-R-font-lock-keywords
              ess-R-readline nil ;; with --no-readline argument.
              ;; ess-R-font-lock-keywords
              )

;;; set prompt
;;
;; .Rprofile 
;; Make the ℝ prompt stand out (be sure to tell ESS how to handle this): 
;; options(prompt="ℝ> ")
;;
(setq inferior-ess-primary-prompt "ℝ> ")
;; Handle the custom ℝ prompt in ess. Don’t use custom here.
(setq inferior-S-prompt "[]a-zA-Z0-9.[]*\\(?:[>+.] \\)*ℝ+> ")

;; TODO: test whether ESS by default already set this. if not, enable this configuration.
;; and check out the prettify result in R source file.
;; (setq prettify-symbols-alist '(("->" . 8594)
;;                                ("<-" . 8592)
;;                                ("->>" . 8608)
;;                                ("<<-" . 8606)))


;; start the ESS process if not started when open ESS buffers.
;; (add-hook 'after-init-hook 'R)

(define-key my-inferior-ess-map (kbd "r") 'R)


;;; completing support

;; - `ess-company-backends' :: for company-mode.
;; - `ess-ac-sources' :: for auto-complete.

;; R
;;   (ess-company-backends . '((company-R-args company-R-objects)))



;;; TODO: temporally fix lintr missing for flycheck issue.

(add-hook 'R-mode-hook
          (lambda ()
            (flycheck-mode -1)))



(provide 'init-my-prog-lang-R)

;;; init-my-prog-lang-R.el ends here
