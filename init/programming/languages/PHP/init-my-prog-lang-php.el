;;; init-my-prog-lang-php.el --- init for PHP
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ php-mode ]

(use-package php-mode
  :ensure t
  :mode (("[.]php$" . php-mode)
         ("\\(pages\\|snippets\\|templates\\)/.*[.]php?$" . web-mode))
  :init
  (with-eval-after-load 'php-mode
    (require 'php-ext))
  :config
  (setq indent-tabs-mode nil
        c-basic-offset 2
        php-template-compatibility nil
        php-search-documentation-browser-function t
        )
  )

;;; [ inf-php ]

(use-package inf-php
  :ensure t)

;;; [ php-boris ]

;; (use-package php-boris
;;   :ensure t
;;   :defer t)

;;; [ php-boris-minor-mode ]

;; (use-package php-boris-minor-mode
;;   :ensure t
;;   :defer t
;;   :init
;;   (add-hook 'php-mode-hook 'php-boris-minor-mode)
;;   )

;;; [ php-eldoc ]

(use-package php-eldoc
  :ensure t
  :init
  (add-hook 'php-mode-hook 'php-eldoc-enable))

;;; [ ac-php ] -- auto-completion and company source for php for GNU Emacs.

(use-package ac-php
  :ensure t
  :bind (:map php-mode-map
              ("M-." . ac-php-find-symbol-at-point)
              ("M-," . ac-php-location-stack-back)
              ("C-h d d" . ac-php-show-tip))
  :config
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
              ))
  )


;;; [ ob-php ] -- execute PHP within org-mode blocks.

;; (use-package ob-php
;;   :ensure t)


;;; [ phpunit ]

;; (use-package phpunit
;;   :ensure t
;;   :init
;;   (with-eval-after-load 'php-mode
;;     (define-key php-mode-map (kbd "C-c t t") 'phpunit-current-test)
;;     (define-key php-mode-map (kbd "C-c t c") 'phpunit-current-class)
;;     (define-key php-mode-map (kbd "C-c t p") 'phpunit-current-project))
;;   )


(provide 'init-my-prog-lang-php)

;;; init-my-prog-lang-php.el ends here
