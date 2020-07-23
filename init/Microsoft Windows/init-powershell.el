;;; init-powershell.el --- init for PowerShell. -*- lexical-binding: t; -*-

;;; Time-stamp: <2020-02-15 21:44:16 stardiviner>

;;; Commentary:



;;; Code:

(use-package powershell
  :ensure t
  :commands (powershell))

(use-package ob-powershell
  :load-path (lambda () (expand-file-name "init/extensions/ob-powershell.el" user-emacs-directory))
  :defer t
  :commands (org-babel-execute:powershell)
  :init (setq org-babel-powershell-command "pwsh")
  :config
  (add-to-list 'org-babel-load-languages '(powershell . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("powershell" . "ps1"))

  ;; (add-to-list 'org-babel-default-header-args:powershell
  ;;              '(:eval . "yes"))
  ;; (add-to-list 'org-babel-default-header-args:powershell
  ;;              '(:noweb . "yes"))
  ;; (add-to-list 'org-babel-default-header-args:powershell
  ;;              '(:results . "output"))
  ;; (add-to-list 'org-babel-default-header-args:powershell
  ;;              '(:session . "*???*")) ; TODO:
  )




(provide 'init-powershell)

;;; init-powershell.el ends here
