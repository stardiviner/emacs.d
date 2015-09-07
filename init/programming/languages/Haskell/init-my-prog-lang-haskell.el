;;; init-my-prog-lang-haskell.el --- init Haskell for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ haskell-mode ]

(require 'haskell-mode)

(eval-after-load 'haskell-mode
  '(progn
     (add-hook 'haskell-mode-hook
               (lambda ()
                 (turn-on-haskell-doc-mode)
                 (turn-on-haskell-indentation)

                 (run-haskell)
                 ))))


;;; [ flycheck-haskell ]

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))


;;; [ company-ghc ]

(dolist (hook '(haskell-mode-hook
                ))
  (add-hook hook
            (lambda ()
              (add-to-list (make-local-variable 'company-backends)
                           'company-cabal)
              (add-to-list (make-local-variable 'company-backends)
                           'company-ghc)
              (add-to-list (make-local-variable 'company-backends)
                           'company-ghci)
              )))


;;; [ company-ghci ]


;;; [ ebal ] -- Emacs interface to Cabal.



;;; [ company-cabal ]



(provide 'init-my-prog-lang-haskell)

;;; init-my-prog-lang-haskell.el ends here
