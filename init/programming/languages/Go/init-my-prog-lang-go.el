;;; init-my-prog-lang-go.el --- init Go language Emacs settings

;;; Commentary:


;;; Code:

;;; [ go-mode ]

(require 'go-mode)



;;; [ go-eldoc ]

(require 'go-eldoc) ; Don't need to require, if you install by package.el
(add-hook 'go-mode-hook 'go-eldoc-setup)



;;; [ go-autocomplete ]


;;; [ go-play ]



(provide 'init-my-prog-lang-go)

;;; init-my-prog-lang-go.el ends here
