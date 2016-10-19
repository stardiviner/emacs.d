;;; init-my-prog-lang-php.el --- init for PHP
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ php-mode ]

(use-package php-mode
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'php-mode
    (require 'php-ext))

  (add-to-list 'auto-mode-alist '("[.]php$" . php-mode))
  (add-to-list 'auto-mode-alist
               '("\\(pages\\|snippets\\|templates\\)/.*[.]php?$" . web-mode))
  :config
  (setq indent-tabs-mode nil
        c-basic-offset 2
        php-template-compatibility nil
        )
  
  (setq php-search-documentation-browser-function t)
  )


;;; [ inf-php ]

(use-package inf-php
  :ensure t
  :defer t)


;;; [ php-boris ]

(use-package php-boris
  :ensure t
  :defer t)


;;; [ php-boris-minor-mode ]

(use-package php-boris-minor-mode
  :ensure t
  :defer t
  :init
  (add-hook 'php-mode-hook 'php-boris-minor-mode)
  )


;;; [ ac-php ] -- auto-completion and company source for php for GNU Emacs.

(use-package ac-php
  :ensure t
  :defer t
  :init
  ;; fix ac-php duplicate parentheses with `smartparens-mode'.
  (add-to-list 'sp-ignore-modes-list 'php-mode)
  
  (add-hook 'php-mode-hook
            (lambda ()
              ;; company-mode
              ;; (my-company-add-backend-locally 'company-ac-php-backend)

              ;; auto-complete
              ;; (setq ac-sources '(ac-source-php))
              (add-to-list 'ac-sources 'ac-source-php-template)
              (add-to-list 'ac-sources 'ac-source-php)
              
              (smartparens-mode 1)
              ))
  
  ;; keybindings
  ;; goto define & go back
  (define-key php-mode-map (kbd "M-.") 'ac-php-find-symbol-at-point)
  (define-key php-mode-map (kbd "M-,") 'ac-php-location-stack-back)
  (define-key php-mode-map (kbd "C-h d d") 'ac-php-show-tip)
  )


;;; [ php-eldoc ]

(use-package php-eldoc
  :ensure t
  :defer t
  :init
  (add-hook 'php-mode-hook 'php-eldoc-enable))


;;; [ ob-php ] -- execute PHP within org-mode blocks.

(use-package ob-php
  :ensure t)


;;; [ phpunit ]

(use-package phpunit
  :ensure t
  :defer t
  :init
  (define-key php-mode-map (kbd "C-c t t") 'phpunit-current-test)
  (define-key php-mode-map (kbd "C-c t c") 'phpunit-current-class)
  (define-key php-mode-map (kbd "C-c t p") 'phpunit-current-project)
  )


(provide 'init-my-prog-lang-php)

;;; init-my-prog-lang-php.el ends here
