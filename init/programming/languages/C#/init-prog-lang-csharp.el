;;; init-prog-lang-csharp.el --- init for C#
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ csharp-mode ]

(use-package csharp-mode
  :ensure t
  :ensure-system-package mono)

;;; [ omnisharp ] -- C# IDE for Emacs.

(use-package omnisharp
  :ensure t
  :config
  ;; (setq omnisharp-server-executable-path
  ;;       (expand-file-name "~/Code/CSharp/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe"))
  (setq omnisharp-company-begin-after-member-access t
        omnisharp-company-do-template-completion t
        omnisharp-company-template-use-yasnippet t
        omnisharp-company-ignore-case t
        ;; auto-complete
        omnisharp-auto-complete-want-documentation nil
        omnisharp-auto-complete-popup-persist-help nil
        ;; eldoc
        omnisharp-eldoc-support t
        ;; imenu
        omnisharp-imenu-support nil
        )

  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  
  (defun my-omnisharp-setup ()
    (interactive)
    (my-company-add-backend-locally 'company-omnisharp)
    (define-key omnisharp-mode-map (kbd "M-.") 'omnisharp-go-to-definition)
    )
  
  (add-hook 'omnisharp-mode-hook 'my-omnisharp-setup)
  )

;;; [ ob-csharp ]

(use-package ob-csharp
  :quelpa (ob-csharp :fetcher github :repo "thomas-villagers/ob-csharp"
                     :files ("src/ob-csharp.el"))
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


(provide 'init-prog-lang-csharp)

;;; init-prog-lang-csharp.el ends here
