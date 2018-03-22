;;; init-prog-lang-R.el --- init R

;;; Commentary:


;;; Code:

;;; [ Emacs Speaks Statistics (ESS) ]

(use-package ess
  :ensure t
  :ensure-system-package (R . "sudo pacman -S --noconfirm r")
  :defer t
  :load (ess-site)
  :mode (("\\.[rR]\\'" . R-mode)
         ("\\.Rd\\'" . Rd-mode) ; R documentation
         ("\\.S\\'" . S-mode)
         ("\\.Rprofile\\'" . R-mode)
         ("\\.Renviron\\'" . R-mode))
  :commands (R)
  :config
  ;; (setq ess-ask-for-ess-directory nil) ; suppress ESS from prompting for session directory.
  
  ;; with --no-readline argument.
  (setq ess-R-readline nil)

  ;; set prompt
  ;; (setq inferior-ess-primary-prompt "ℝ> ")
  ;; handle the custom ℝ prompt in ess. Don’t use custom here.
  ;; (setq inferior-S-prompt "[]a-zA-Z0-9.[]*\\(?:[>+.] \\)*ℝ+> ")

  ;; completing support
  ;; - `ess-company-backends' :: for company-mode.
  ;; - `ess-ac-sources' :: for auto-complete.

  (setq ess-use-ido t
        ess-ido-flex-matching t
        ess-pdf-viewer-pref '("emacsclient")
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

  ;; quickly insert assign operator: <-
  (define-key ess-mode-map (kbd "C-c =") (lambda () (interactive) (insert " <- ")))

  (add-to-list 'display-buffer-alist
               '("\\*help\\[R\\]\\(.*\\)\\*" . (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist
               '("\\*ess-apropos\\[R\\]\\(.*\\)\\*" . (display-buffer-below-selected)))
  )

;;; [ ess-view ] -- View R dataframes in a spreadsheet software.

(use-package ess-view
  :ensure t
  :defer t)

;;; [ ess-R-data-view ] -- data viewer for GNU R. It shows dataframe and matrix on table view.

(use-package ess-R-data-view
  :ensure t
  :defer t)


;;; [ ob-R ]

(require 'ob-R)

(add-to-list 'org-babel-load-languages '(R . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(add-to-list 'org-babel-tangle-lang-exts '("R" . "R"))

(setq org-babel-default-header-args:R
      '((:session . "*R*")
        (:exports . "both")
        (:results . "replace")
        ;; customize R plot window
        ;; (:width . 640)
        ;; (:height . 640)
        ;; (:bg . "white")
        ;; (:type . :any)
        ))


(provide 'init-prog-lang-R)

;;; init-prog-lang-R.el ends here
