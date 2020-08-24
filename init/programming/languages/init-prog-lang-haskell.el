;;; init-prog-lang-haskell.el --- init Haskell for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ haskell-mode ]

(use-package haskell-mode
  :ensure t
  :defer t
  :commands (run-haskell haskell-interactive-switch haskell-interactive-bring)
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
              ([f5] . haskell-compile))
  :defer t
  :config
  (setq haskell-font-lock-symbols t
        haskell-stylish-on-save nil
        haskell-tags-on-save nil)

  ;; [ Haskell Interactive Mode ]
  (require 'haskell-interactive-mode)
  (require 'haskell-process)
  (require 'inf-haskell)

  ;; (setq haskell-process-type 'auto) ; 'ghci

  ;; (setq haskell-process-type 'stack-ghci)
  
  ;; fix collision with `ob-haskell' session.
  ;; (defun haskell-session-default-name ()
  ;;   "Generate a default project name for the new project prompt."
  ;;   (let ((file (haskell-cabal-find-file)))
  ;;     (or (when file
  ;;           (downcase (file-name-sans-extension
  ;;                      (file-name-nondirectory file))))
  ;;         "*haskell*")))
  
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

  ;; (add-hook 'haskell-mode-hook 'interactive-haskell-mode)

  ;; [ completion ]
  ;; remove `haskell-interactive-mode-completion-at-point-function' from `completion-at-point-functions'.
  (defun company-haskell-fix-company-capf ()
    (setq-local completion-at-point-functions
                (delq 'haskell-completions-completion-at-point
                      completion-at-point-functions)))
  (add-hook 'haskell-mode-hook #'company-haskell-fix-company-capf)
  
  ;; [ indent ]
  (add-hook 'haskell-mode-hook #'haskell-doc-mode)
  
  ;; [ module ]
  ;; (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

  ;; Doc (Haddocks)
  (require 'w3m-haddock)
  ;; make haddock pages a little more palatable (and add syntax highlighting to source view)
  (add-hook 'w3m-display-hook 'w3m-haddock-display)
  (setq haskell-w3m-haddock-dirs '("~/.cabal/share/doc/"))
  (define-key haskell-mode-map (kbd "C-c d w") 'haskell-w3m-open-haddock)

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

  ;; auto newline after insert specific characters.
  (defun haskell-mode-electric-layout-setting ()
    (add-to-list 'electric-layout-rules
                 '((?\{) (?\} . around))))
  (add-hook 'haskell-mode-hook #'electric-layout-local-mode)
  (add-hook 'haskell-mode-hook #'haskell-mode-electric-layout-setting))


;;; [ hindent ] -- Haskell indent.

;; (use-package hindent
;;   :ensure t
;;   :init (add-hook 'haskell-mode-hook #'hindent-mode))


;;; [ flycheck-haskell ]

;; (use-package flycheck-haskell
;;   :ensure t
;;   :init
;;   (with-eval-after-load 'flycheck
;;     (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))
;;   )

;;; [ ob-haskell ]

(use-package ob-haskell
  :defer t
  :commands (org-babel-execute:haskell)
  :config
  (add-to-list 'org-babel-load-languages '(haskell . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("haskell" . "hs"))
  (add-to-list 'org-babel-default-header-args:haskell '(:session . "*haskell*")))

;;; [ ghc ]

(use-package ghc
  :ensure t
  :defer t
  :init
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  (add-hook 'haskell-mode-hook #'ghc-init)
  :config
  ;; if you wish to display error each goto next/prev error,
  (setq ghc-display-error 'minibuffer)
  ;; (setq ghc-debug t)
  )

;;; [ lsp-haskell ] -- Haskell support for lsp-mode.

(use-package lsp-haskell
  :ensure t
  :after lsp
  :hook (haskell-mode . lsp))

;;; [ company-ghc ] -- company-mode back-end for haskell-mode via ghc-mod.

(use-package company-ghc
  :ensure t
  :defer t
  :init
  ;; [ company-ghci ] -- company backend which uses the current ghci process.
  (use-package company-ghci
    :ensure t)
  (defun my/haskell-company-backends-setup ()
    (interactive)
    (my-company-add-backend-locally 'company-ghc)
    (my-company-add-backend-locally 'company-ghci))
  (add-hook 'haskell-mode-hook #'my/haskell-company-backends-setup)
  :custom ((company-ghc-show-info t)
           (company-ghc-show-module t)))

;;; [ company-cabal ] -- company-mode back-end for haskell-cabal-mode.

;; (use-package company-cabal
;;   :ensure t
;;   :defer t)


;;; [ intero ] -- Complete interactive development program for Haskell.

;; (use-package intero
;;   :ensure t
;;   :init
;;   (add-hook 'haskell-mode-hook 'intero-mode)
;;   )

;;; [ dante ] -- a fork of Intero.

;; (use-package dante
;;   :ensure t
;;   :commands 'dante-mode
;;   :init
;;   (add-hook 'haskell-mode-hook 'dante-mode)
;;   ;; (add-hook 'haskell-mode-hook 'flycheck-mode)
;;   :config
;;   ;; fix org-mode babel haskell src block [C-c '] keybinding been overwrite issue.
;;   (define-key dante-mode-map (kbd "C-c '") 'org-edit-src-exit)
;;   (define-key dante-mode-map (kbd "C-x C-e") 'dante-eval-block)
;;   )

;;; [ scion ] -- IDE library for Haskell based on the GHC API.

;; (use-package scion
;;   :ensure t
;;   :init
;;   (add-hook 'haskell-mode-hook
;;             (lambda ()
;;               (scion-mode 1)
;;               (scion-flycheck-on-save 1)))
;;   :config
;;   ;; (setq scion-program "~/.cabal/bin/scion-server")
;;   ;; (setq scion-completing-read-function 'ido-completing-read)
;;
;;   (add-hook 'scion-connected-hook
;;             (lambda ()
;;               (notifications-notify :title "Haskell Scion connected"
;;                                     :body "Haskell Scion connected.")))
;;   )


(provide 'init-prog-lang-haskell)

;;; init-prog-lang-haskell.el ends here
