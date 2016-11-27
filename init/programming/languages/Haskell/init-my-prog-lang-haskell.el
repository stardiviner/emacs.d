;;; init-my-prog-lang-haskell.el --- init Haskell for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ haskell-mode ]

(use-package haskell-mode
  :ensure t
  :defer t
  :bind (:map haskell-mode-map
              ("C-c C-s" . haskell-interactive-bring)
              ("C-c C-k" . haskell-interactive-mode-clear)
              ("M-." . haskell-mode-jump-to-def-or-tag)
              ("C-c C-p C-l" . haskell-process-load-or-reload)
              ("C-c C-p C-t" . haskell-process-do-type)
              ("C-c C-p C-i" . haskell-process-do-info)
              ("C-c C-p C-c" . haskell-process-cabal-build)
              ("C-c C-p M-c" . haskell-process-cabal)
              ([f8] . haskell-navigate-imports)
              ([f5] . haskell-compile)

              :map my-prog-inferior-map
              ("h" . haskell-interactive-switch)
              )
  :init
  (setq haskell-font-lock-symbols t
        haskell-stylish-on-save nil
        haskell-tags-on-save nil
        )
  :config
  ;; [ Haskell Interactive Mode ]
  (require 'haskell-interactive-mode)
  (require 'haskell-process)
  
  ;; (setq haskell-process-type 'auto ; 'cabal-repl
  ;;       ;; haskell-process-args-cabal-repl '("--ghc-option=-ferror-spans"
  ;;       ;;                                   "--with-ghc=ghci-ng"
  ;;       ;;                                   )
  ;;       haskell-process-suggest-remove-import-lines t
  ;;       haskell-process-suggest-haskell-docs-imports t
  ;;       haskell-process-suggest-hoogle-imports t
  ;;       haskell-process-auto-import-loaded-modules t
  ;;       haskell-process-log t
  ;;       haskell-process-use-presentation-mode t
  ;;       haskell-interactive-mode-eval-mode 'haskell-mode
  ;;       haskell-complete-module-preferred '("Data.ByteString"
  ;;                                           "Data.ByteString.Lazy"
  ;;                                           "Data.Conduit"
  ;;                                           "Data.Function"
  ;;                                           "Data.List"
  ;;                                           "Data.Map"
  ;;                                           "Data.Maybe"
  ;;                                           "Data.Monoid"
  ;;                                           "Data.Ord")
  ;;       )

  ;; [ indent ]
  (add-hook 'haskell-mode-hook #'haskell-doc-mode)
  
  ;; [ module ]
  ;; (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

  ;; Doc (Haddocks)
  (require 'w3m-haddock)
  ;; make haddock pages a little more palatable (and add syntax highlighting to source view)
  (add-hook 'w3m-display-hook 'w3m-haddock-display)
  (setq haskell-w3m-haddock-dirs '("~/.cabal/share/doc/"))
  (define-key haskell-mode-map (kbd "C-c C-d") 'haskell-w3m-open-haddock)

  ;; API search (use Hayoo)
  (define-key haskell-mode-map (kbd "C-h d d") 'haskell-hayoo)
  
  ;; auto start `inf-haskell'
  (defun my-haskell-interactive-start ()
    (interactive)
    (unless (get-buffer "*haskell*")
      (haskell-interactive-bring)
      (switch-to-buffer "*haskell*")
      (bury-buffer)))

  ;; (add-hook 'haskell-mode-hook 'my-haskell-interactive-start)
  )


;;; [ hindent ] -- Haskell indent.

(use-package hindent
  :ensure t
  :defer t
  :init
  (add-hook 'haskell-mode-hook #'hindent-mode)
  )


;;; [ flycheck-haskell ]

;; (use-package flycheck-haskell
;;   :ensure t
;;   :defer t
;;   :init
;;   (with-eval-after-load 'flycheck
;;     (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))
;;   )


;;; [ ghc ]

(use-package ghc
  :ensure t
  :init
  ;; (setq ghc-debug t)
  (add-hook 'haskell-mode-hook #'ghc-init)
  :config
  ;; if you wish to display error each goto next/prev error,
  (setq ghc-display-error 'minibuffer)
  )


;;; [ company-ghc ] -- company-mode back-end for haskell-mode via ghc-mod.

(use-package company-ghc
  :ensure t
  :init
  (add-hook 'haskell-mode-hook
            (lambda ()
              (my-company-add-backend-locally 'company-ghc)))
  :config
  (setq company-ghc-show-info t
        company-ghc-show-module t
        )
  )


;;; [ company-ghci ] -- company backend which uses the current ghci process.

;; (use-package company-ghci
;;   :ensure t
;;   :defer t
;;   :init
;;   (add-hook 'haskell-mode-hook
;;             (lambda ()
;;               (my-company-add-backend-locally 'company-ghci)
;;               )))


;;; [ company-cabal ] -- company-mode back-end for haskell-cabal-mode.

;; (use-package company-cabal
;;   :ensure t
;;   :defer t)



;; (defun my-haskell-company-backends-setup ()
;;   "Setup `company-backends' for Haskell related modes."
;;   (interactive)
;;   (add-to-list (make-local-variable 'company-backends)
;;                '(company-ghc
;;                  :with
;;                  company-yasnippet
;;                  company-ghci
;;                  ;; company-cabal
;;                  ))
;;   )
;;
;; (dolist (hook '(haskell-mode-hook
;;                 haskell-interactive-mode-hook
;;                 interactive-haskell-mode-hook
;;                 ;; inferior-haskell-mode-hook (deprecated)
;;                 ))
;;   (add-hook hook #'my-haskell-company-backends-setup))


;;; [ intero ] -- Complete interactive development program for Haskell.

;; (use-package intero
;;   :ensure t
;;   :defer t
;;   :init
;;   (add-hook 'haskell-mode-hook 'intero-mode)
;;   )

;;; [ dante ] -- a fork of Intero.

(use-package dante
  :ensure t
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode))


;;; [ scion ] -- IDE library for Haskell based on the GHC API.

(use-package scion
  :ensure t
  :defer t
  :init
  (add-hook 'haskell-mode-hook
            (lambda ()
              (scion-mode 1)
              (scion-flycheck-on-save 1)))

  :config
  ;; (setq scion-program "~/.cabal/bin/scion-server")
  ;; (setq scion-completing-read-function 'ido-completing-read)

  (add-hook 'scion-connected-hook
            (lambda ()
              (notifications-notify :title "Haskell Scion connected"
                                    :body "Haskell Scion connected.")))
  )


;;; [ ebal ] -- Emacs interface to Cabal and Stack.

(use-package ebal
  :ensure t
  :defer t)


(provide 'init-my-prog-lang-haskell)

;;; init-my-prog-lang-haskell.el ends here
