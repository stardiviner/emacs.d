;;; init-my-prog-lang-ESS.el --- init for ESS.
;;; -*- coding: utf-8 -*-

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
;;   - [M-x ESS RET] :: start ESS session.
;;   - [M-x S RET] :: start S session.
;; - [C-c C-s] :: `ess-switch-process', switch/create inferior process
;; - `ess-load-file' :: [C-c C-l], load source code file for completion.

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
      inferior-ess-same-window nil
      ;; ESS Command
      ;; inferior-R-objects-command "print(objects(pos=%d, all.names=TRUE), max=1e6)"
      ;; inferior-Splus-objects-command "objects(where=%d)"
      ;; inferior-ess-get-prompt-command "options()$prompt"
      ;; inferior-ess-r-help-command ".ess.help(\"%s\", help.type=\"text\")"
      )


;;; completing support

;; - `ess-company-backends' :: for company-mode.
;; - `ess-ac-sources' :: for auto-complete.

;; ESS use this sexp to modify company-backends.
;;   (copy-list (append ess-company-backends company-backends))

;; Julia
;;   (ess-company-backends . '(company-julia-objects))



;;; TODO: startup start ESS process


;;; [ ess-smarter-underscore ]

(eval-after-load "ess-site"
  '(progn
     (require 'ess-smart-underscore)))


(provide 'init-my-prog-lang-ESS)

;;; init-my-prog-lang-ESS.el ends here
