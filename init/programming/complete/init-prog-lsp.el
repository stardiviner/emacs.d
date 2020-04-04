;;; init-prog-lsp.el --- init for Language Server Protocol

;;; Commentary:



;;; Code:

;;; [ eglot ] -- Client for Language Server Protocol (LSP) servers.

;; (use-package eglot
;;   :hook (prog-mode . eglot-ensure))

;;; [ lsp-mode ] -- clients for servers using Language Server Protocol.

(use-package lsp-mode
  :ensure t
  :defer t
  :hook ((js-mode css-mode web-mode) . lsp)
  :commands (lsp lsp-describe-session)
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point)
              ("M-RET"   . lsp-execute-code-action))
  :load (lsp-clients) ; load `lsp-clients' for auto configuration of language server clients.
  :init
  ;; speed-up lsp-mode performance
  (setq lsp-log-io nil ; for for debug
        lsp-enable-folding nil
        lsp-diagnostic-package :none ; no real-time syntax check
        ;; lsp-enable-snippet nil ; handle yasnippet by myself
        lsp-enable-symbol-highlighting nil
        lsp-enable-links nil)

  ;; `which-key' integration
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)

  ;; Support LSP in Org Babel with header argument `:file'.
  ;; https://github.com/emacs-lsp/lsp-mode/issues/377
  (defvar org-babel-lsp-explicit-lang-list
    '("java")
    "Org Mode Babel languages which need explicitly specify header argument :file.")
  (cl-defmacro lsp-org-babel-enbale (lang)
    "Support LANG in org source code block."
    ;; (cl-check-type lang symbolp)
    (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
           (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
      `(progn
         (defun ,intern-pre (info)
           (let* ((lsp-file (or (->> info caddr (alist-get :file))
                                (unless (member ,lang org-babel-lsp-explicit-lang-list)
                                  (concat (org-babel-temp-file (format "lsp-%s-" ,lang))
                                          (cdr (assoc ,lang org-babel-tangle-lang-exts)))))))
             (setq-local buffer-file-name lsp-file)
             (setq-local lsp-buffer-uri (lsp--path-to-uri lsp-file))
             (lsp)))
         (if (fboundp ',edit-pre)
             (advice-add ',edit-pre :after ',intern-pre)
           (progn
             (defun ,edit-pre (info)
               (,intern-pre info))
             (put ',edit-pre 'function-documentation
                  (format "Add LSP info to Org source block dedicated buffer (%s)."
                          (upcase ,lang))))))))

  (defvar org-babel-lsp-lang-list
    '("shell"
      ;; "python" "ipython"
      ;; "ruby"
      "js" "css" "html"
      ;; "C" "C++"
      "java" "rust" "go" "kotlin"))

  (dolist (lang org-babel-lsp-lang-list)
    (eval `(lsp-org-babel-enbale ,lang)))

  (defun my/babel-insert-lsp-file-header-argument (lsp-mirror-file)
    "A helper command to insert `:file' header argument path."
    (interactive "fPath to file: ")
    (org-babel-insert-header-arg "file" (format "\"%s\"" lsp-mirror-file)))
  (define-key org-babel-map (kbd "M-f") 'my/babel-insert-lsp-file-header-argument)

  ;; manage lsp-mode popup buffers
  (add-to-list 'display-buffer-alist
               '("^\\*lsp-help\\*" (display-buffer-below-selected)))
  :config
  ;; don't scan 3rd party javascript libraries
  (push "[/\\\\][^/\\\\]*\\.\\(json\\|html\\|jade\\)$" lsp-file-watch-ignored))

;; [ lsp-ui ] -- UI modules for lsp-mode.

(use-package lsp-ui
  :ensure t
  :defer t
  :after lsp
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom ((lsp-ui-doc-enable nil)
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

;;; [ dap-mode ] -- Debug Adapter Protocol mode for lsp-mode.

(use-package dap-mode
  :ensure t
  :ensure posframe ; fix void `posframe-hide' in `dap-ui-controls-mode'
  :after lsp
  :commands (dap-debug)
  :bind (:map dap-mode-map
              ("<f5>" . dap-debug)
              ("<f7>" . dap-step-in)
              ("<M-f7>" . dap-step-out)
              ("<f8>" . dap-next)
              ("<f9>" . dap-continue)))

;;; [ helm-lsp ] -- LSP helm integration.

(use-package helm-lsp
  :ensure t
  :commands (helm-lsp-workspace-symbol helm-lsp-global-workspace-symbol))

;;; [ lsp-docker ] -- lsp-mode uses lsp-docker to run language servers using in Docker containers.

(use-package lsp-docker
  :ensure t)


(provide 'init-prog-lsp)

;;; init-prog-lsp.el ends here
