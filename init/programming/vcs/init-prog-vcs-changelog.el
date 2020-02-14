;;; init-prog-vcs-changelog.el ---  -*- lexical-binding: t; -*-

;;; Time-stamp: <2020-02-14 14:19:16 stardiviner>

;;; Commentary:



;;; Code:

;;; [ add-log ] -- change log maintenance commands for Emacs. [C-x 4 a]

(use-package add-log
  :config
  (defun my:change-log-mode-setup ()
    (setq-local fill-column 72)
    (auto-fill-mode t)
    (setq-local company-dabbrev-code-modes '(text-mode magit-diff-mode))
    (setq-local company-dabbrev-ignore-buffers
                #'my:company-dabbrev-ignore-except-magit-diff)
    (setq company-dabbrev-code-other-buffers 'all)
    (flyspell-mode)
    (setq-local company-backends
                '(company-dabbrev-code
                  :with company-abbrev                  
                  :separate company-ispell)))
  (add-hook 'change-log-mode-hook #'my:change-log-mode-setup))



(provide 'init-prog-vcs-changelog)

;;; init-prog-vcs-changelog.el ends here
