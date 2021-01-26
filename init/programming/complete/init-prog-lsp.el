;;; init-prog-lsp.el --- init for Language Server Protocol

;;; Commentary:



;;; Code:

;;; [ eglot ] -- Client for Language Server Protocol (LSP) servers.

;; (use-package eglot
;;   :ensure t
;;   :hook (prog-mode . eglot-ensure))

;;; [ lsp-mode ] -- clients for servers using Language Server Protocol.

(use-package lsp-mode
  :ensure t
  :defer t
  :commands (lsp lsp-deferred lsp-describe-session)
  ;; :hook (prog-mode . lsp-deferred)
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point)
              ("M-RET"   . lsp-execute-code-action))
  :custom (;; (lsp-auto-configure nil) ; disable auto prepend `company-capf' backend.
           ;; (lsp-client-packages '())
           ;; speed-up lsp-mode performance
           (lsp-log-io nil)             ; for Debug
           (lsp-enable-folding nil)
           (lsp-diagnostics-provider :none) ; no real-time syntax check
           ;; (lsp-enable-snippet nil) ; handle yasnippet by myself
           (lsp-enable-symbol-highlighting nil)
           ;; (lsp-enable-links nil)
           )
  :init (add-to-list 'display-buffer-alist '("^\\*lsp.*\\*" . (display-buffer-below-selected)))
  :config
  ;; disable some lsp clients
  ;; (add-to-list 'lsp-disabled-clients 'ccls)
  ;; (add-to-list 'lsp-disabled-clients '(emacs-lisp-mode . nil))
  ;; (add-to-list 'lsp-disabled-clients '(web-mode . angular-ls))

  (when (featurep 'which-key)
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))
  
  ;; don't scan 3rd party javascript libraries
  (push "[/\\\\][^/\\\\]*\\.\\(json\\|html\\|jade\\)$" lsp-file-watch-ignored)

  ;; Fix ‘lsp-org’ error: flycheck-add-mode: lsp is not a valid syntax checker.
  (with-eval-after-load 'lsp-diagnostics
    (flycheck-define-generic-checker 'lsp
      "A syntax checker using the Language Server Protocol (LSP)
provided by lsp-mode.
See https://github.com/emacs-lsp/lsp-mode."
      :start #'lsp-diagnostics--flycheck-start
      :modes '(lsp-placeholder-mode) ;; placeholder
      :predicate (lambda () lsp-mode)
      :error-explainer (lambda (e)
                         (cond ((string-prefix-p "clang-tidy" (flycheck-error-message e))
                                (lsp-cpp-flycheck-clang-tidy-error-explainer e))
                               (t (flycheck-error-message e)))))))

;; [ lsp-ui ] -- UI modules for lsp-mode.

(use-package lsp-ui
  :ensure t
  :commands (lsp-ui-mode)
  :hook (lsp-mode . lsp-ui-mode)
  :custom ((lsp-ui-doc-enable t)
           (lsp-ui-doc-header nil)
           (lsp-ui-doc-include-signature t)
           (lsp-ui-doc-position 'at-point)
           (lsp-ui-sideline-show-hover nil)
           (lsp-ui-sideline-enable nil))
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions) ; [M-.]
              ([remap xref-find-references] . lsp-ui-peek-find-references)   ; [M-?]
              ("C-c C-j" . lsp-ui-imenu))
  :preface (if (featurep 'xwidget-internal) (setq lsp-ui-doc-use-webkit t)))

;;; [ lsp-ivy ] -- LSP Ivy integration.

(use-package lsp-ivy
  :ensure t
  :commands (lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol)
  :bind (:map lsp-command-map ("g G" . lsp-ivy-workspace-symbol)))

;;; [ lsp-treemacs ] -- LSP treemacs

(use-package lsp-treemacs
  :ensure t
  ;; :hook (lsp-mode . lsp-treemacs-sync-mode)
  :commands (lsp-treemacs-errors-list
             lsp-treemacs-quick-fix
             lsp-treemacs-symbols-list
             lsp-treemacs-references
             lsp-treemacs-implementations
             lsp-treemacs-call-hierarchy
             lsp-treemacs-type-hierarchy
             lsp-treemacs-deps-list))

;;; [ dap-mode ] -- Debug Adapter Protocol mode for lsp-mode.

(use-package dap-mode
  :ensure t
  :after lsp
  :commands (dap-debug dap-hydra)
  :bind (:map dap-mode-map
              ("<f5>" . dap-debug)
              ("<f7>" . dap-step-in)
              ("<M-f7>" . dap-step-out)
              ("<f8>" . dap-next)
              ("<f9>" . dap-continue))
  ;; enable only some features
  :custom (dap-auto-configure-features '(sessions locals controls tooltip))
  :init (dap-mode t) (dap-ui-mode t)
  :config
  (defun dap-mode-call-dap-hydra ()
    (call-interactively #'dap-hydra))
  (add-hook 'dap-stopped-hook #'dap-mode-call-dap-hydra))

;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;;; [ lsp-docker ] -- lsp-mode uses lsp-docker to run language servers using in Docker containers.

(use-package lsp-docker
  :ensure t
  :defer t)

;;; [ lsp-sonarlint ] -- SonarLint™ is free IDE extension that lets you fix coding issues before they exist!

;; (use-package lsp-sonarlint
;;   :ensure t
;;   :custom ((lsp-sonarlint-server-path (expand-file-name
;;                                        "lsp/lsp-sonarlint/server/sonarlint-language-server.jar"
;;                                        user-emacs-directory)))
;;   :config
;;   (require 'lsp-sonarlint-python)
;;   (setq lsp-sonarlint-python-enabled t)
;;   (setq lsp-sonarlint-python-analyzer-path
;;         (expand-file-name "lsp/lsp-sonarlint/languages" user-emacs-directory))
;;
;;   (require 'lsp-sonarlint-javascript)
;;   (setq lsp-sonarlint-javascript-enabled t)
;;   (setq lsp-sonarlint-javascript-analyzer-path
;;         (expand-file-name "lsp/lsp-sonarlint/languages" user-emacs-directory))
;;
;;   (require 'lsp-sonarlint-java)
;;   (setq lsp-sonarlint-java-enabled t)
;;   (setq lsp-sonarlint-java-analyzer-path
;;         (expand-file-name "lsp/lsp-sonarlint/languages" user-emacs-directory))
;;
;;   (require 'lsp-sonarlint-html)
;;   (setq lsp-sonarlint-html-enabled t)
;;   (setq lsp-sonarlint-html-analyzer-path
;;         (expand-file-name "lsp/lsp-sonarlint/languages" user-emacs-directory))
;;
;;   (require 'lsp-sonarlint-php)
;;   (setq lsp-sonarlint-php-enabled t)
;;   (setq lsp-sonarlint-php-analyzer-path
;;         (expand-file-name "lsp/lsp-sonarlint/languages" user-emacs-directory)))


(provide 'init-prog-lsp)

;;; init-prog-lsp.el ends here
