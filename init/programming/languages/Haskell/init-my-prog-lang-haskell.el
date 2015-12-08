;;; init-my-prog-lang-haskell.el --- init Haskell for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ haskell-mode ]

(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook
            '(lambda ()
               (turn-on-haskell-doc-mode)
               (turn-on-haskell-indentation)
               ;; disable `aggressive-indent-mode' in `haskell-mode'.
               (aggressive-indent-mode -1)
               ))

  (define-key my-prog-inferior-map (kbd "h") 'run-haskell)
  )


;;; [ flycheck-haskell ]

(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))


;;; [ company-ghc ]

(use-package company-ghc
  :config
  (setq company-ghc-show-info t
        company-ghc-show-module t
        ))


;;; [ company-ghci ]


;;; [ company-cabal ]


(dolist (hook '(haskell-mode-hook
                inferior-haskell-mode
                ))
  (add-hook hook
            '(lambda ()
               (my-company-add-backends-to-mode
                '(company-ghc
                  company-ghci
                  company-cabal))
               )))


;;; [ ghc ]

(use-package ghc)


;;; [ ghci-completion ]

(use-package ghci-completion)


;;; [ ebal ] -- Emacs interface to Cabal.

(use-package ebal)


(provide 'init-my-prog-lang-haskell)

;;; init-my-prog-lang-haskell.el ends here
