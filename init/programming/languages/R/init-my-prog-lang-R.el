;;; init-my-prog-lang-R.el --- init R

;;; Commentary:


;;; Code:

;;; [ Emacs Speaks Statistics (ESS) ]

;; It is designed to support editing of scripts and interaction with various
;; statistical analysis programs such as R, S-Plus, SAS, Stata and JAGS.

;;; S family
;; (R, S+, (aka S-PLUS)).

;;; Usage:
;;
;; - [M-x ess-version] :: check out ESS version.
;; - Start an ESS process.
;;   - [M-x S RET] :: start S session.

;; (add-to-list 'load-path "~/.emacs.d/el-get/ess/lisp/")
;; (load "ess-site")

(require 'ess-site)
(require 'ess-eldoc)


(setq ess-use-ido t
      ess-ido-flex-matching t
      ess-blink-region t
      ess-blink-delay 0.3
      ;; ess-pdf-viewer-pref '("okular" "--unique")
      ;; ess-ps-viewer-pref nil
      ess-speedbar-use-p t              ; use speedbar
      ;; ESS Edit
      ess-auto-newline nil
      ;; ess-default-style
      ess-fancy-comments t
      ;; ess-mode-silently-save
      ess-tab-always-indent t
      ;; ESS Extra
      ess-use-eldoc t
      ess-eldoc-show-on-symbol t
      ess-eldoc-abbreviation-style 'aggressive ; t or 'aggressive,
      ess-describe-at-point-method 'tooltip  ; display in a tooltip. (need to press [C-c C-d C-e]
      ;; ess-use-tracebug t
      ;; ESS Help
      ;; alist of frame parameters used to create help frames.
      ess-help-frame-alist '((height . 14) (width . 80) (unsplittable . t))
      ess-help-kill-bogus-buffers t
      ess-help-own-frame nil
      ess-help-pop-to-buffer t
      ess-help-reuse-window t
      ;; ESS Proc
      ess-eval-deactivate-mark t
      ess-eval-visibly nil ; speedup eval without show the eval commands.
      ess-eval-visibly-at-end t
      ess-execute-in-process-buffer nil
      ess-synchronize-evals nil
      ess-verbose nil
      ess-use-R-completion t
      inferior-ess-own-frame nil
      inferior-ess-same-window t
      ;; ESS Command
      ;; inferior-R-objects-command "print(objects(pos=%d, all.names=TRUE), max=1e6)"
      ;; inferior-Splus-objects-command "objects(where=%d)"
      ;; inferior-ess-get-prompt-command "options()$prompt"
      ;; inferior-ess-r-help-command ".ess.help(\"%s\", help.type=\"text\")"
      )



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

;; TODO: test whether ESS by default already set this. if not, enable this configuration.
;; and check out the prettify result in R source file.
;; (setq prettify-symbols-alist '(("->" . 8594)
;;                                ("<-" . 8592)
;;                                ("->>" . 8608)
;;                                ("<<-" . 8606)))


;;; [ ESS + auto-complete ]

;;; From version 12.03 ESS integrates out of the box with auto-complete package.
;;;
;;; Three sources ‘ac-source-R-args’, ‘ac-source-R-objects’ and ‘ac-source-R’
;;; are included in ESS distribution. The latest combines previous two and makes
;;; them play nicely together.

(setq ess-use-auto-complete t ; use auto-complete.
      ess-tab-complete-in-script nil ; not just complete in ESS buffers.
      )

(add-hook 'R-mode-hook
          (lambda ()
            ;; setup for auto-complete
            (setq ac-sources '(ac-source-R ac-source-R-objects ac-source-R-args))
            ))

;; start the ESS process if not started when open ESS buffers.
;; (add-hook 'after-init-hook 'R)

(define-key my-inferior-ess-map (kbd "r") 'R)


;;; [ ac-R ]

;; (require 'ac-R)




(provide 'init-my-prog-lang-R)

;;; init-my-prog-lang-R.el ends here
