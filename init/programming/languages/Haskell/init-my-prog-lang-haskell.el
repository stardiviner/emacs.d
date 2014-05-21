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


(provide 'init-my-prog-lang-haskell)

;;; init-my-prog-lang-haskell.el ends here
