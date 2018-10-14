;;; init-microsoft-windows.el --- init for Microsoft Windows.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
;;; [ ini-mode ] -- Major mode for Windows-style .ini files.

(use-package ini-mode
  :ensure t
  :mode "\\.ini\\'")

;;; [ bat-mode ] -- Emacs built-in major mode for editing DOS/Windows scripts.

(require 'bat-mode)

;;; [ bmx-mode ] -- Batch Mode eXtras.

(use-package bmx-mode
  :ensure t
  :config
  ;; (bmx-mode-setup-defaults)
  (add-hook 'bat-mode-hook #'bmx-mode)
  (defun my/bmx-mode-company-setup ()
    (make-local-variable 'company-backends)
    (add-to-list 'company-backends #'bmx--company-label-backend)
    (add-to-list 'company-backends #'bmx--company-variable-backend)
    (add-hook 'company-completion-finished-hook #'bmx--company-completion-finished-hook))
  (add-hook 'bmx-mode-hook #'my/bmx-mode-company-setup))

;;; [ PowerShell ]

(use-package powershell
  :ensure t
  :commands (powershell)
  :init
  ;; (setq powershell-location-of-exe
  ;;       "c:\\windows\\system32\\WindowsPowerShell\\v1.0\\powershell.exe")
  (setq powershell-location-of-exe "/usr/bin/pwsh"))

(use-package ob-powershell
  :load-path "~/.emacs.d/init/extensions/ob-powershell.el"
  :init (require 'ob-powershell)
  (setq org-babel-powershell-command "pwsh")
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

;;; [ .NET ]

(use-package dotnet
  :ensure t
  :commands (dotnet-add-package
             dotnet-new
             dotnet-run dotnet-run-with-args
             dotnet-build
             dotnet-publish
             dotnet-test dotnet-test-rerun
             dotnet-sln-new dotnet-sln-add dotnet-sln-remove dotnet-sln-list))

;;; [ AutoHotkey ]

(use-package ahk-mode
  :ensure t)

;;; ----------------------------------------------------------------------------

(provide 'init-microsoft-windows)

;;; init-microsoft-windows.el ends here
