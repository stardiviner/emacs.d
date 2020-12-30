;;; init-prog-lang-php.el --- init for PHP
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ php-mode ] -- Major mode for editing PHP code.

;; (use-package php-mode
;;   :ensure t
;;   :defer t
;;   :mode (("\\.php\\'" . php-mode)
;;          ("\\(pages\\|snippets\\|templates\\)/.*\\.php?$" . web-mode))
;;   :custom ((indent-tabs-mode nil)
;;            (c-basic-offset 2)
;;            (php-template-compatibility nil)
;;            (php-search-documentation-browser-function t)))

;;; [ phps-mode ] -- Major mode for PHP with Semantic integration.

(use-package phps-mode
  :ensure t
  :defer t
  :mode ("\\.php\\'" "\\.phtml\\'")
  :custom ((phps-mode-async-process t)                  ; Asynchronous lexer
           (phps-mode-async-process-using-async-el nil) ; Asynchronous lexer via threads
           ;; (phps-mode-async-process-using-async-el t) ; Asynchronous lexer via async.el processes
           )
  :config (phps-mode-flycheck-setup))

;;; [ php-eldoc ]

(use-package php-eldoc
  :ensure t
  :ensure auto-complete
  :defer t
  :init (add-hook 'php-mode-hook 'php-eldoc-enable))

;;; [ ob-php ] -- execute PHP within org-mode blocks.

(use-package ob-php
  :defer t
  :commands (org-babel-execute:php)
  :config
  (add-to-list 'org-babel-load-languages '(php . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("php" . "php")))


(provide 'init-prog-lang-php)

;;; init-prog-lang-php.el ends here
