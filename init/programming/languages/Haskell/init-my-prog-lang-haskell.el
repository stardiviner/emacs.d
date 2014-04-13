;;; init-my-prog-lang-haskell.el --- init Haskell for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ haskell-mode ]

(require 'haskell-mode)

(eval-after-load 'haskell-mode
  '(progn
     (defun my-haskell-mode-defaults ()
       (turn-on-haskell-doc-mode)
       (turn-on-haskell-indentation))

     (setq my-haskell-mode-hook 'my-haskell-mode-defaults)

     (add-hook 'haskell-mode-hook
               (lambda ()
                 (run-hooks 'my-haskell-mode-hook)))))


(provide 'init-my-prog-lang-haskell)

;;; init-my-prog-lang-haskell.el ends here
