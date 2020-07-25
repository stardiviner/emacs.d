;;; init-prog-vcs-changelog.el ---  -*- lexical-binding: t; -*-

;;; Time-stamp: <2020-07-19 11:43:20 stardiviner>

;;; Commentary:



;;; Code:

;;; [ add-log ] -- change log maintenance commands for Emacs. [C-x 4 a]

(use-package add-log
  ;; :custom (add-log-keep-changes-together t)
  :config
  (defun my:change-log-mode-setup ()
    (setq-local fill-column 72)
    (auto-fill-mode t)
    (setq-local company-dabbrev-code-modes '(text-mode magit-diff-mode))
    (setq-local company-dabbrev-ignore-buffers
                #'my:company-dabbrev-ignore-except-magit-diff)
    (setq company-dabbrev-code-other-buffers 'all)
    (setq-local company-backends
                '(company-dabbrev-code
                  :with company-abbrev                  
                  :separate company-ispell)))
  (add-hook 'change-log-mode-hook #'my:change-log-mode-setup))

;;; [ magit-patch-changelog ] -- Generate a patch according to emacs-devel CONTRIBUTE guidelines.

(use-package magit-patch-changelog
  :ensure t
  :after magit
  :demand t)



(provide 'init-prog-vcs-changelog)

;;; init-prog-vcs-changelog.el ends here
