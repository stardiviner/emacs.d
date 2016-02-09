;;; init-my-prog-lang-php.el --- init for PHP
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ php-mode ]

(use-package php-mode
  :ensure t
  :config
  (setq php-search-documentation-browser-function t)
  )


;;; [ php-eldoc ]

(use-package php-eldoc
  :ensure t
  :config
  (php-eldoc-enable))


;;; [ inf-php ]

(use-package inf-php
  :ensure t
  :config
  )


;;; [ php-boris ]


;;; [ php-boris-minor-mode ]


;;; [ phpunit ]



(provide 'init-my-prog-lang-php)

;;; init-my-prog-lang-php.el ends here
