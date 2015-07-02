;;; init-my-prog-lang-R.el --- init R

;;; Commentary:


;;; Code:

;;; To make speedbar show an R buffer’s functions, variables, etc, you need to
;;; add R to the set of supported extensions;
;;; This also work with sr-speedbar.
(speedbar-add-supported-extension ".R")


;;; [ R-mode ]

;;; Usage:
;;
;; - [M-x R] :: start R process.
;; - 'ess-R-describe-object-at-point-commands' / [C-c C-d C-e] ::
;; - [C-c C-e C-t] runs the command `ess-build-tags-for-directory'

(autoload 'R-mode "ess-site" "R-mode" t)

;; automatically get the correct mode
(add-to-list 'auto-mode-alist '("\\.[rR]$" . R-mode))
(add-to-list 'auto-mode-alist '("\\.Rd$" . Rd-mode)) ; R documentation
(add-to-list 'auto-mode-alist '("\\.S$" . S-mode))

(add-to-list 'auto-mode-alist '("\\.Rprofile$" . R-mode))
(add-to-list 'auto-mode-alist '("\\.Renviron$" . R-mode))

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


;;; [ ESS + company-mode ]

(require 'company-ess)

;; global
;; (add-to-list 'company-backends 'company-ess)

;; local to ESS mode
(add-hook 'ess-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends)
                         'company-ess)))


;;; [ ESS + auto-complete ]

;;; From version 12.03 ESS integrates out of the box with auto-complete package.
;;;
;;; Three sources ‘ac-source-R-args’, ‘ac-source-R-objects’ and ‘ac-source-R’
;;; are included in ESS distribution. The latest combines previous two and makes
;;; them play nicely together.

;; (setq ess-use-auto-complete t ; use auto-complete.
;;       ess-tab-complete-in-script nil ; not just complete in ESS buffers.
;;       )

;; (eval-after-load 'auto-complete
;;   '(progn
;;      (add-hook 'R-mode-hook
;;                (lambda ()
;;                  ;; setup for auto-complete
;;                  (setq ac-sources '(ac-source-R ac-source-R-objects ac-source-R-args))
;;                  ))))


;; start the ESS process if not started when open ESS buffers.
;; (add-hook 'after-init-hook 'R)

(define-key my-inferior-ess-map (kbd "r") 'R)


;;; TODO: startup start ESS process



;;; [ ac-R ]

;; (require 'ac-R)




;;; [ Julia ]

(autoload 'julia-mode "ess-site" "Julia mode" t)




(provide 'init-my-prog-lang-R)

;;; init-my-prog-lang-R.el ends here
