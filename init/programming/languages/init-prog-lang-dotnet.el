;;; init-prog-lang-dotnet.el --- init for .NET
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ dotnet ] -- Interact with dotnet CLI tool.

(use-package dotnet ; dotnet-mode keymap prefix [C-c C-n]
  :ensure t
  :ensure-system-package dotnet-runtime
  :ensure-system-package dotnet-host
  :ensure-system-package dotnet-sdk
  :defer t
  :commands (dotnet-new dotnet-add-package dotnet-test dotnet-command))

;;; [ csproj-mode ] -- Work with .NET project files (csproj, vbproj).

(use-package csproj-mode
  :ensure t
  :defer t)

;;; [ csharp-mode ] -- C# mode derived mode.

(use-package csharp-mode
  :ensure t
  :ensure-system-package mono
  :defer t
  :mode ("\\.cs\\'" . csharp-mode))

;;; [ omnisharp ] -- Omnicompletion (intellisense) and more for C#.

(use-package omnisharp
  :ensure t
  :defer t
  :init (setq omnisharp-server-executable-path
              (expand-file-name "~/Code/.NET/omnisharp-roslyn/run"))
  (setq omnisharp-company-begin-after-member-access t
        omnisharp-company-do-template-completion t
        omnisharp-company-template-use-yasnippet t
        omnisharp-company-ignore-case t
        omnisharp-auto-complete-want-documentation nil
        omnisharp-auto-complete-popup-persist-help nil
        omnisharp-eldoc-support t
        omnisharp-imenu-support nil)
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  :config
  (defun my-omnisharp-setup ()
    (my-company-add-backend-locally 'company-omnisharp)
    (define-key omnisharp-mode-map (kbd "M-.") 'omnisharp-go-to-definition))
  (add-hook 'omnisharp-mode-hook 'my-omnisharp-setup)

  (add-to-list 'display-buffer-alist
               '("^\\* OmniSharp : Usages \\*" (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist
               '("^\\* OmniSharp : Implementations \\*" (display-buffer-below-selected)))
  )

;;; [ ob-csharp ] -- org-babel functions for csharp evaluation.

(use-package ob-csharp
  :quelpa (ob-csharp :fetcher github :repo "thomas-villagers/ob-csharp"
                     :files ("src/ob-csharp.el"))
  :defer t
  :commands (org-babel-execute:csharp)
  :init (setq org-babel-csharp-compiler "mcs")
  :config
  (add-to-list 'org-babel-load-languages '(csharp . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("csharp" . "cs"))

  ;; (add-to-list 'org-babel-default-header-args:csharp
  ;;              '(:eval . "yes"))
  ;; (add-to-list 'org-babel-default-header-args:csharp
  ;;              '(:noweb . "yes"))
  ;; (add-to-list 'org-babel-default-header-args:csharp
  ;;              '(:results . "output"))
  ;; (add-to-list 'org-babel-default-header-args:csharp
  ;;              '(:session . "*???*")) ; TODO:
  )

;;; [ fsharp-mode ] -- F# mode for Emacs.

(use-package fsharp-mode
  :ensure t)

;;; [ ob-fsharp ] -- Org-mode Babel support for F#.

(use-package ob-fsharp
  :ensure t
  :defer t
  :commands (org-babel-execute:fsharp)
  :config
  (add-to-list 'org-babel-load-languages '(fsharp . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))


(provide 'init-prog-lang-dotnet)

;;; init-prog-lang-dotnet.el ends here
