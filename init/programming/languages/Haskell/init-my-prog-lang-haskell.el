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


;;; [ company-ghc ]

(dolist (hook '(haskell-mode-hook
                ))
  (add-hook hook
            (lambda ()
              ;; mode locally for company backends
              (add-to-list (make-local-variable 'company-backends)
                           'company-ghc)
              (add-to-list (make-local-variable 'company-backends)
                           'company-ghci)
              (add-to-list (make-local-variable 'company-backends)
                           'company-cabal)
              )))


;;; [ company-ghci ]


;;; [ company-cabal ]



(provide 'init-my-prog-lang-haskell)

;;; init-my-prog-lang-haskell.el ends here
