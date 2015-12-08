;;; init-my-prog-lang-haskell.el --- init Haskell for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ haskell-mode ]

(use-package haskell-mode
  :config
  (setq haskell-stylish-on-save nil)
  
  (add-hook 'haskell-mode-hook
            '(lambda ()
               (turn-on-haskell-doc-mode)
               ;; indent
               (turn-on-haskell-simple-indent)
               (turn-on-haskell-indent)
               (turn-on-haskell-indentation)
               ;; disable `aggressive-indent-mode' in `haskell-mode'.
               (aggressive-indent-mode -1)
               ))

  
  ;; [ Haskell Interactive Mode ]
  (require 'haskell-interactive-mode)
  (require 'haskell-process)
  
  (setq haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t
        haskell-process-log t
        haskell-tags-on-save t)

  (add-hook 'haskell-mode-hook
            '(lambda ()
               ;; inferior-haskell-mode (deprecated)
               ;; (inf-haskell-mode 1)
               ;; Haskell Interactive Mode
               (interactive-haskell-mode 1)
               ))
  
  (define-key my-prog-inferior-map (kbd "h") 'haskell-interactive-switch)

  (define-key haskell-mode-map (kbd "C-c C-s") 'haskell-interactive-bring)
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-mode-map (kbd "C-c M-c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
  
  ;; (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def)
  ;; (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)
  ;; To use GHCi first and then if that fails to fallback to tags for jumping
  (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)
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

(use-package ghc
  :config
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  (add-hook 'haskell-mode-hook '(lambda () (ghc-init)))

  ;; if you wish to display error each goto next/prev error,
  (setq ghc-display-error 'minibuffer)
  )


;;; [ ghci-completion ]

(use-package ghci-completion)


;;; [ ebal ] -- Emacs interface to Cabal.

(use-package ebal)


(provide 'init-my-prog-lang-haskell)

;;; init-my-prog-lang-haskell.el ends here
