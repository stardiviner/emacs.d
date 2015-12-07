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

               (run-haskell)
               ))
  )


;;; [ flycheck-haskell ]

(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))


;;; [ company-ghc ]
;;; [ company-ghci ]
;;; [ company-cabal ]

(dolist (hook '(haskell-mode-hook
                inferior-haskell-mode
                ))
  (add-hook hook
            '(lambda ()
               (add-to-list (make-local-variable 'company-backends)
                            'company-cabal)
               (add-to-list (make-local-variable 'company-backends)
                            'company-ghc)
               (add-to-list (make-local-variable 'company-backends)
                            'company-ghci)
               )))


;;; [ ghc ]

(use-package ghc)


;;; [ ghci-completion ]

(use-package ghci-completion)


;;; [ ebal ] -- Emacs interface to Cabal.

(use-package ebal)


(provide 'init-my-prog-lang-haskell)

;;; init-my-prog-lang-haskell.el ends here
