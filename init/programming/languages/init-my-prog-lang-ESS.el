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
;; - [C-c C-d e] :: `ess-describe-object-at-point'

(use-package ess
  :ensure t
  :config
  (require 'ess-site)

  (setq ess-use-ido t
        ess-ido-flex-matching t
        ess-pdf-viewer-pref '("zauthura")
        ;; ess-ps-viewer-pref nil
        ;; ESS Edit
        ess-auto-newline t
        ;; ESS Extra
        ess-describe-at-point-method 'tooltip
        ;; ESS Help
        ;; alist of frame parameters used to create help frames.
        ;; ess-help-frame-alist '((height . 14) (width . 80) (unsplittable . t))
        ess-help-own-frame nil
        ess-help-pop-to-buffer t
        ess-help-reuse-window t
        ;; ESS Proc
        ess-eval-visibly nil ; speedup eval without show the eval commands.
        ess-eval-visibly-at-end t
        ess-execute-in-process-buffer nil
        )

  ;; completing support
  ;; - `ess-company-backends' :: for company-mode.
  ;; - `ess-ac-sources' :: for auto-complete.
  (setq ess-use-company nil)
  (setq ess-use-auto-complete t)
  (add-hook 'ess-mode-hook
            '(lambda ()
               (auto-complete-mode 1)
               (company-mode -1)))
  )



(provide 'init-my-prog-lang-ESS)

;;; init-my-prog-lang-ESS.el ends here
