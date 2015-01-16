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
      ess-tab-complete-in-script nil
      ess-use-auto-complete t
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
      ess-eval-visibly t
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
      ;; R
      ;; ess-R-font-lock-keywords
      ess-R-readline nil ;; with --no-readline argument.
      ;; inferior-R-args ""
      ;; inferior-R-font-lock-keywords
      )



;;; [ R-mode ]

(autoload 'R-mode "ess" "ESS" t)

;; automatically get the correct mode
(add-to-list 'auto-mode-alist
             '("\\.R$" . R-mode)
             '("\\.S$" . S-mode)
             '("\\.Rd\\" . Rd-mode) ; R documentation
             )
;; comment out the following if you are not using R/S-Plus on ACPUB
;; add a ";" in front of each line
;; (load "/usr/pkg/ess/lisp/ess-site")
;; (setq-default inferior-S+6-program-name "Splus")

(setq-default inferior-R-program-name "R") ;; /path/to/R

(setq ess-eval-visibly-p t)
;; (setq ess-ask-for-ess-directory t)


;;; [ ess-eldoc ]
(require 'ess-eldoc)


;;; [ auto-complete-acr ]

;;; auto-complete-mode extension for GNU R(statistics)
;; https://github.com/myuhe/auto-complete-acr.el






(provide 'init-my-prog-lang-R)

;;; init-my-prog-lang-R.el ends here
