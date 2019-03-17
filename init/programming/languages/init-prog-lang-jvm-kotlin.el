;;; init-prog-lang-jvm-kotlin.el --- init for Kotlin
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ kotlin-mode ]

(use-package kotlin-mode
  :ensure t)

;;; [ ob-kotlin ]

(use-package ob-kotlin
  :ensure t
  :defer t
  :commands (org-babel-execute:kotlin)
  :config
  (add-to-list 'org-babel-load-languages '(kotlin . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("kotlin" . "kt")))


(provide 'init-prog-lang-jvm-kotlin)

;;; init-prog-lang-jvm-kotlin.el ends here
