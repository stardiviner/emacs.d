;;; init-my-prog-lang-csharp.el --- init for C#
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
;;; [ csharp-mode ]

(use-package csharp-mode
  :ensure t)

;;; [ omnisharp ] -- C# IDE for Emacs.

(use-package omnisharp
  :ensure t
  :defer t
  :init
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  :config
  (add-hook 'omnisharp-mode-hook
            (lambda ()
              (my-company-add-backend-locally 'company-omnisharp)))
  )

;;; ----------------------------------------------------------------------------

(provide 'init-my-prog-lang-csharp)

;;; init-my-prog-lang-csharp.el ends here
