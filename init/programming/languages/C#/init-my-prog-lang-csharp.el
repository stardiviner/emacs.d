;;; init-my-prog-lang-csharp.el --- init for C#
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ csharp-mode ]

(use-package csharp-mode
  :ensure t
  :ensure-system-package (mono))

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


(provide 'init-my-prog-lang-csharp)

;;; init-my-prog-lang-csharp.el ends here
