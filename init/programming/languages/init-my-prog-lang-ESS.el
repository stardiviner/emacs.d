;;; init-my-prog-lang-ESS.el --- init for ESS.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Emacs Speaks Statistics (ESS) ]

(use-package ess
  :ensure t
  :defer t
  :init
  (require 'ess-site)
  
  ;; auto start ESS inferior process
  ;; (add-hook 'ess-mode-hook #'ess-force-buffer-current)

  :config
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
  )



(provide 'init-my-prog-lang-ESS)

;;; init-my-prog-lang-ESS.el ends here
