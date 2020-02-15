;;; init-prog-lang-dotnet.el --- init for .NET
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ .NET ] -- Interact with dotnet CLI tool.

(use-package dotnet ; dotnet-mode keymap prefix [C-c C-n]
  :ensure t
  :defer t
  :commands (dotnet-add-package
             dotnet-new
             dotnet-run dotnet-run-with-args
             dotnet-build
             dotnet-publish
             dotnet-test dotnet-test-rerun
             dotnet-sln-new dotnet-sln-add dotnet-sln-remove dotnet-sln-list)
  :config
  (add-to-list 'display-buffer-alist
               '("^\\*dotnet\\*" (display-buffer-below-selected))))

;;; [ csproj-mode ] -- Work with .NET project files (csproj, vbproj).

(use-package csproj-mode
  :ensure t
  :defer t)

;;; [ csharp-mode ] -- C# mode derived mode.

(use-package csharp-mode
  :ensure t
  :defer t
  :mode ("\\.cs\\'" . csharp-mode)
  :config
  (defun my:csharp-mode-setup ()
    (setq-local indent-tabs-mode nil)
    (setq-local c-syntactic-indentation t)
    (c-set-style "ellemtel")
    (setq-local c-basic-offset 4)
    (setq-local truncate-lines t)
    (setq-local tab-width 4)
    (electric-pair-local-mode 1))
  (add-hook 'csharp-mode-hook 'my:csharp-mode-setup t)

  (define-key csharp-mode-map (kbd "C-c C-c") 'recompile))

;;; [ omnisharp ] -- Omnicompletion (intellisense) and more for C#.

(use-package omnisharp
  :ensure t
  :defer t
  :bind (:map omnisharp-mode-map
              ("M-." . omnisharp-go-to-definition)
              ("C-c /" . omnisharp-helm-find-symbols)
              ("C-c ," . omnisharp-helm-find-usages)
              ("<M-return>" . omnisharp-run-code-action-refactoring)
              ("C-c C-r" . omnisharp-rename)
              ("C-c C-j" . omnisharp-navigate-to-current-file-member)
              ;; Tests
              ("C-c C-t p" . omnisharp-unit-test-at-point)
              ("C-c C-t b" . omnisharp-unit-test-buffer)
              ("C-c C-t l" . omnisharp-unit-test-last))
  :init (setq omnisharp-server-executable-path
              (let ((omnisharp-run-source (expand-file-name "~/Code/.NET/omnisharp-roslyn/run")) ; source code compiled
                    (omnisharp-run-binary (expand-file-name "~/Code/.NET/omnisharp-linux-x86/run"))) ; pre-compiled binary
                (if (file-exists-p omnisharp-run-source)
                    omnisharp-run-source omnisharp-run-binary)))
  (setq omnisharp-company-begin-after-member-access t
        omnisharp-company-do-template-completion t
        omnisharp-company-template-use-yasnippet t
        omnisharp-company-ignore-case t
        omnisharp-auto-complete-popup-help-delay nil
        omnisharp-auto-complete-want-documentation t
        omnisharp-auto-complete-popup-persist-help t
        omnisharp-eldoc-support t
        omnisharp-imenu-support t
        ;; omnisharp-auto-complete-want-importable-types t
        )
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  :config
  (defun my-omnisharp-setup ()
    (setq-local completion-ignore-case t)
    (my-company-add-backend-locally 'company-omnisharp))
  (add-hook 'omnisharp-mode-hook 'my-omnisharp-setup)

  (add-to-list 'display-buffer-alist
               '("^\\* OmniSharp : Usages \\*" (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist
               '("^\\* OmniSharp : Implementations \\*" (display-buffer-below-selected))))

;;; [ ob-csharp ] -- org-babel functions for csharp evaluation.

(use-package ob-csharp
  :quelpa (ob-csharp :fetcher github :repo "thomas-villagers/ob-csharp" :files ("src/*.el"))
  :commands (org-babel-execute:csharp)
  :init (setq org-babel-csharp-compiler "mcs")
  :config
  (add-to-list 'org-babel-load-languages '(csharp . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("csharp" . "cs")))

;;; [ fsharp-mode ] -- F# mode for Emacs.

(use-package fsharp-mode
  :ensure t
  :defer t)

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
