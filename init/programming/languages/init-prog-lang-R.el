;;; init-prog-lang-R.el --- init R

;;; Commentary:


;;; Code:

;;; [ Emacs Speaks Statistics (ESS) ]

(use-package ess
  :ensure t
  :defer t
  :mode (("\\.[rR]\\'" . R-mode)
         ("\\.Rd\\'" . Rd-mode) ; R documentation
         ("\\.S\\'" . S-mode)
         ("\\.Rprofile\\'" . R-mode)
         ("\\.Renviron\\'" . R-mode))
  :commands (R run-ess-r)
  :custom (;; (ess-ask-for-ess-directory nil) ; suppress ESS from prompting for session directory.
           ;; with --no-readline argument.
           (ess-R-readline nil)
           ;; (inferior-ess-primary-prompt "ℝ> ")
           ;; ;; handle the custom ℝ prompt in ess. Don’t use custom here.
           ;; (inferior-S-prompt "[]a-zA-Z0-9.[]*\\(?:[>+.] \\)*ℝ+> ")
           (ess-use-ido nil)
           (ess-describe-at-point-method 'tooltip)
           (ess-eval-visibly nil) ; speedup eval without show the eval commands.
           (ess-use-company t)
           (ess-use-auto-complete nil))
  :init
  (add-to-list 'display-buffer-alist '("\\*help\\[R\\]\\(.*\\)\\*" . (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist '("\\*ess-apropos\\[R\\]\\(.*\\)\\*" . (display-buffer-below-selected)))
  :config
  ;; auto start ESS inferior process
  ;; (add-hook 'ess-mode-hook #'ess-force-buffer-current)

  ;; quickly insert assign operator: <-
  (with-eval-after-load 'ess-mode
    (define-key ess-mode-map (kbd "C-c =") (lambda () (interactive) (insert " <- ")))))

;;; [ ess-view ] -- View R dataframes in a spreadsheet software.

(use-package ess-view
  :ensure t
  :defer t)

;;; [ ess-R-data-view ] -- data viewer for GNU R. It shows dataframe and matrix on table view.

(use-package ess-R-data-view
  :ensure t
  :defer t)


;;; [ ob-R ]

(use-package ob-R
  :defer t
  :commands (org-babel-execute:R)
  :config
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
          )))


(provide 'init-prog-lang-R)

;;; init-prog-lang-R.el ends here
