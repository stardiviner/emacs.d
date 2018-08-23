;;; init-prog-lang-php.el --- init for PHP
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ php-mode ]

(use-package php-mode
  :ensure t
  :ensure-system-package php
  :defer t
  :mode (("\\.php\\'" . php-mode)
         ("\\(pages\\|snippets\\|templates\\)/.*\\.php?$" . web-mode))
  :load (php-ext)
  :config
  (setq indent-tabs-mode nil
        c-basic-offset 2
        php-template-compatibility nil
        php-search-documentation-browser-function t))

;;; [ php-eldoc ]

(use-package php-eldoc
  :ensure t
  :defer t
  :init (add-hook 'php-mode-hook 'php-eldoc-enable))

;;; [ ob-php ] -- execute PHP within org-mode blocks.

(require 'ob-php)
(add-to-list 'org-babel-load-languages '(php . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(add-to-list 'org-babel-tangle-lang-exts '("php" . "php"))

;;; [ lsp-php ] -- PHP support for lsp-mode.

(use-package lsp-php
  :ensure t
  :init (add-hook 'php-mode-hook #'lsp-php-enable))


(provide 'init-prog-lang-php)

;;; init-prog-lang-php.el ends here
