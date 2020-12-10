;;; init-prog-lang-kotlin.el --- init for Kotlin
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ kotlin-mode ]

(use-package kotlin-mode
  :ensure t
  :defer t
  :mode "\\.kt\\'")

;;; [ ob-kotlin ]

(use-package ob-kotlin
  :ensure t
  :defer t
  :commands (org-babel-execute:kotlin)
  :config
  (add-to-list 'org-babel-load-languages '(kotlin . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("kotlin" . "kt")))

;;; [ flycheck-kotlin ] -- Support kotlin in flycheck.

(use-package flycheck-kotlin
  :ensure t
  :defer t
  :after kotlin-mode
  :commands (flycheck-kotlin-setup)
  :init (add-hook 'kotlin-mode-hook #'flycheck-kotlin-setup))

;;; [ lsp-kotlin ]

(use-package lsp-mode
  :ensure t
  :hook (kotlin-mode . lsp)
  :init (with-eval-after-load 'lsp-clients
          (let ((kotlin-ls-executable (expand-file-name
                                       (concat "~/Code/Kotlin/kotlin-language-server/"
                                               "server/build/install/server/bin/kotlin-language-server"))))
            (setq lsp-clients-kotlin-server-executable kotlin-ls-executable)
            (add-to-list 'exec-path (file-name-directory kotlin-ls-executable)))))


(provide 'init-prog-lang-kotlin)

;;; init-prog-lang-kotlin.el ends here
