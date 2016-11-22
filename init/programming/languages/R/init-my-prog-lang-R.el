;;; init-my-prog-lang-R.el --- init R

;;; Commentary:


;;; Code:

;;; [ Emacs Speaks Statistics (ESS) ]

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
  (setq ess-use-company t)
  (setq ess-use-auto-complete nil)

  (setq ess-use-eldoc t)

  ;; auto start ESS inferior process
  ;; (add-hook 'ess-mode-hook #'ess-force-buffer-current)
  )


;;; [ R-mode ]

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


(unless (boundp 'my-prog-inferior-map)
  (define-prefix-command 'my-prog-inferior-map))

(defun my-run-R-auto ()
  (interactive)
  (my-func/open-and-switch-to-buffer 'R "*R*" t))

(define-key my-prog-inferior-map (kbd "R") 'my-run-R-auto)


;;; completing support

;; - `ess-company-backends' :: for company-mode.
;; - `ess-ac-sources' :: for auto-complete.

;;; [ ob-R ]

(use-package org-plus-contrib
  :ensure t
  :config
  (require 'ob-R))


(provide 'init-my-prog-lang-R)

;;; init-my-prog-lang-R.el ends here
