;;; init-my-prog-lang-clojure.el --- init for Clojure.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ clojure-mode ]

(use-package clojure-mode
  :ensure t
  :defer t
  :config
  (add-hook 'clojure-mode-hook #'my-lisp-common-settings)
  ;; (add-hook 'clojure-mode-hook 'smartparens-strict-mode)
  ;; `subword-mode' is quite useful since we often have to deal with Java class
  ;; and method names.
  (add-hook 'clojure-mode-hook #'subword-mode)

  (add-hook 'clojure-repl-mode-hook #'my-lisp-repl-common-settings)
  
  
  (with-eval-after-load 'clojure-mode
    (font-lock-add-keywords
     'clojure-mode `(("(\\(fn\\)[\[[:space:]]" ; (fn
                      (0 (progn (compose-region (match-beginning 1)
                                                (match-end 1) "λ")
                                nil)))))
    (font-lock-add-keywords
     'clojure-mode `(("\\(#\\)(" ; #(
                      (0 (progn (compose-region (match-beginning 1)
                                                (match-end 1) "ƒ")
                                nil)))))
    (font-lock-add-keywords
     'clojure-mode `(("\\(#\\){" ; #{
                      (0 (progn (compose-region (match-beginning 1)
                                                (match-end 1) "∈")
                                nil)))))
    ;; java.io.BufferedReader. (class constructor)
    (font-lock-add-keywords
     'clojure-mode `(("([^\\..*]*\\(\\.\\)\\{1\\}[\[:space:]\n)]"
                      (0 (progn (compose-region (match-beginning 1)
                                                (match-end 1) "⊿")
                                nil)))))
    )
  )


;;; [ CIDER ] -- CIDER is a Clojure IDE and REPL for Emacs

(use-package cider
  :ensure t
  :config
  (setq cider-auto-mode t
        nrepl-hide-special-buffers nil
        cider-repl-pop-to-buffer-on-connect nil ; buffer will only be created not displayed
        cider-auto-select-error-buffer t
        nrepl-buffer-name-separator " "
        nrepl-buffer-name-show-port nil
        nrepl-log-messages t
        nrepl-prompt-to-kill-server-buffer-on-quit t

        ;; versions
        cider-jack-in-auto-inject-clojure "1.9.0-alpha12"
        ;; cider-minimum-clojure-version "1.8.0"
        ;; cider-required-nrepl-version "0.2.12"

        ;; resources
        cider-prefer-local-resources t

        ;; font-lock
        cider-font-lock-dynamically '(macro core deprecated function)

        ;; indent
        cider-dynamic-indentation t
        
        ;; REPL
        cider-inject-dependencies-at-jack-in t
        cider-repl-display-in-current-window nil
        cider-repl-pop-to-buffer-on-connect nil
        cider-prompt-save-file-on-load t
        cider-repl-result-prefix ";; => "
        cider-repl-use-clojure-font-lock t
        ;; cider-repl-tab-command 'cider-repl-indent-and-complete-symbol
        ;; cider-known-endpoints '(("host-a" "10.10.10.1" "7888") ("host-b" "7888"))
        cider-repl-use-pretty-printing t
        cider-repl-wrap-history t
        cider-repl-history-size 500
        cider-repl-history-file nil
        cider-show-error-buffer t

        ;; pretty-printing
        cider-pprint-fn 'fipp
        
        ;; Eval
        cider-show-eval-spinner t
        cider-use-overlays 'both
        cider-overlays-use-font-lock t ; use overlay for results.
        cider-result-use-clojure-font-lock t
        cider-eval-result-duration nil

        ;; Enlighten

        ;; Fringe linter
        cider-use-fringe-indicators t

        ;; mouse over tooltips
        cider-use-tooltips t
        
        ;; Compilation
        cider-auto-jump-to-error 'errors-only
        cider-auto-select-error-buffer t

        ;; clojure.test
        cider-test-show-report-on-success nil

        ;; stacktraces
        cider-show-error-buffer t
        cider-auto-select-error-buffer t
        cider-stacktrace-default-filters '(tooling dup)

        ;; debugging

        ;; code reloading
        cider-refresh-show-log-buffer t

        ;; multiple connections
        cider-request-dispatch 'dynamic

        ;; Mode Line
        cider-mode-line-show-connection t

        ;; project
        cider-prompt-for-project-on-connect 'when-needed

        ;; build tool
        ;; cider-preferred-build-tool "boot"
        )

  ;; Complete & annotations
  (setq cider-completion-use-context t
        cider-annotate-completion-candidates t
        ;; cider-completion-annotations-include-ns 'always ; 'unqualified
        cider-completion-annotations-alist '(("class" "c")
                                             ("field" "fi")
                                             ("function" "λ") ; f, λ, ƒ
                                             ("import" "i")
                                             ("keyword" "k")
                                             ("local" "l")
                                             ("macro" "♪") ; ♪, ➜
                                             ("method" "m")
                                             ("namespace" "ns")
                                             ("protocol" "p")
                                             ("protocol-function" "p-λ")
                                             ("record" "r")
                                             ("special-form" "s")
                                             ("static-field" "sf")
                                             ("static-method" "sm")
                                             ("type" "t")
                                             ("var" "v"))
        )

  ;; Java

  ;; Enlighten faces
  ;; TODO: better way to enable
  ;; (add-hook 'cider-connected-hook
  ;;           (lambda ()
  ;;             (sleep-for 60)
  ;;             (cider-enlighten-mode 1)))
  
  (set-face-attribute 'cider-enlightened-local-face nil
                      :foreground "yellow" :background "#333333"
                      :family "DejaVu Sans Mono"
                      :italic t
                      :box '(:color "#444444" :line-width 1)
                      )
  (set-face-attribute 'cider-enlightened-face nil
                      :foreground "dark orange" :background "#333333"
                      :family "DejaVu Sans Mono"
                      :weight 'normal
                      :box '(:color "#444444" :line-width 1)
                      )

  ;; eval sexp result overlays
  (set-face-attribute 'cider-result-overlay-face nil
                      :foreground "light gray"
                      :background (color-darken-name (face-background 'default) 2)
                      :box '(:line-width -1 :color "dim gray")
                      :family "DejaVu Sans Mono"
                      )
  
  ;; auto completion with company-mode support
  ;; `cider-complete-at-point' in `completion-at-point-functions'
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)

  ;; enable `eldoc' in relevant buffers.
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (setq cider-eldoc-display-for-symbol-at-point t ; NOTE: enable this will cause high CPU.
        cider-eldoc-max-class-names-to-display 3)

  (add-hook 'cider-repl-mode-hook #'subword-mode)

  ;; enable `cider-mode' in `clojure-mode'.
  (add-hook 'clojure-mode-hook #'cider-mode)
  
  ;; auto start CIDER jack-in.
  (setq cider-allow-jack-in-without-project t)
  (add-hook 'after-init-hook #'cider-jack-in)

  ;; notify user CIDER is connected.
  (add-hook 'cider-connected-hook
            (lambda ()
              (notifications-notify :title "CIDER connected"
                                    :body "CIDER process connected.")))
  
  ;; switch to CIDER REPL buffers.
  (defun my-cider-clojure-repl-switch ()
    (interactive)
    (let ((cider-clojure-repl-buffer-name "*cider-repl localhost*"))
      (if (get-buffer cider-clojure-repl-buffer-name)
          (switch-to-buffer cider-clojure-repl-buffer-name)
        (cider-jack-in)
        )))
  
  (defun my-cider-cljs-repl-switch ()
    (interactive)
    (let ((cider-cljs-repl-buffer-name "*cider-repl CLJS localhost*"))
      (if (get-buffer cider-cljs-repl-buffer-name)
          (switch-to-buffer cider-cljs-repl-buffer-name)
        ;; kill the old CIDER Clojure REPL process
        (kill-process (get-process "nrepl-connection"))
        (kill-process (get-process "nrepl-server"))
        (setq cider-cljs-lein-repl
              "(do (require 'figwheel-sidecar.repl-api)
                   (figwheel-sidecar.repl-api/start-figwheel!)
                   (figwheel-sidecar.repl-api/cljs-repl))")
        (cider-jack-in-clojurescript))))
  
  (unless (boundp 'my-inferior-lisp-map)
    (define-prefix-command 'my-inferior-lisp-map))
  (define-key my-inferior-lisp-map (kbd "c") 'my-cider-clojure-repl-switch)

  (define-key clojure-mode-map (kbd "C-c C-s") 'my-cider-clojure-repl-switch)
  (define-key clojurescript-mode-map (kbd "C-c C-s") 'my-cider-cljs-repl-switch)
  
  ;; CIDER inspect command keybindings
  (unless (boundp 'cider-inspect-prefix)
    (define-prefix-command 'cider-inspect-prefix))

  (add-hook 'clojure-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c i") 'cider-inspect-prefix)
              (define-key cider-inspect-prefix (kbd "r") 'cider-inspect-last-result)
              (define-key cider-inspect-prefix (kbd "E") 'cider-inspect-expr)
              (define-key cider-inspect-prefix (kbd "d") 'cider-inspect-defun-at-point)
              (define-key cider-inspect-prefix (kbd "e") 'cider-inspect-last-sexp)
              (define-key cider-inspect-prefix (kbd "i") 'cider-inspect-read-and-inspect)
              ))
  )


;;; run test when load file.

(defun cider-tdd-test ()
  "Thin wrapper around `cider-test-run-tests'."
  (when (cider-connected-p)
    (let ((cider-auto-select-test-report-buffer nil)
          (cider-test-show-report-on-success nil))
      (cider-test-run-ns-tests nil)
      (message "CIDER TDD test run successfully."))))

(define-minor-mode cider-tdd-mode
  "Run all tests whenever a file is saved."
  t nil nil
  :global t
  (if cider-tdd-mode
      (add-hook 'cider-file-loaded-hook #'cider-tdd-test)
    (remove-hook 'cider-file-loaded-hook #'cider-tdd-test)))

;; (set-face-attribute 'cider-test-success-face nil
;;                     :foreground "green" :background nil)

;; (add-hook 'clojure-mode-hook 'cider-tdd-mode)


;;; [ flycheck-clojure, squiggly-clojure ] --

(use-package flycheck-clojure
  :ensure t
  :init
  (with-eval-after-load 'flycheck
    (flycheck-clojure-setup))
  )


;;; [ clj-refactor ]

(use-package clj-refactor
  :ensure t
  :config
  ;; (setq cljr-warn-on-eval nil)
  (setq cljr-auto-sort-ns nil)

  ;; do not prefer prefixes when using clean-ns
  (setq cljr-favor-prefix-notation nil)

  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode 1)
              ;; insert keybinding setup here
              (cljr-add-keybindings-with-prefix "M-RET")
              ))

  ;; skip Tab in `cljr-add-require-to-ns' snippet.
  ;; (advice-add 'cljr-add-require-to-ns :after
  ;;             (lambda (&rest _)
  ;;               (yas-next-field)
  ;;               (yas-next-field)))
  )


;;; [ cider-decompile ] -- decompilation extension for cider.

;; (use-package cider-decompile
;;   :ensure t
;;   :bind (:map clojure-mode-map
;;               ("??" . cider-decompile-func)
;;               ("??" . cider-decompile-ns-func))
;;   )


;;; [ cider-spy ] -- Spy on CIDER to get Info.

;; (use-package cider-spy
;;   :ensure t
;;   :defer t)


;;; [ cider-profile ]

;; (use-package cider-profile
;;   :ensure t
;;   :defer t
;;   :init
;;   (add-hook 'cider-mode-hook 'cider-profile-mode)
;;   (add-hook 'cider-repl-mode-hook 'cider-profile-mode)
;;   )


;;; [ typed-clojure-mode ] -- Typed Clojure minor mode for Emacs.

;; (use-package typed-clojure-mode
;;   :ensure t
;;   :defer t
;;   :init
;;   (add-hook 'clojure-mode-hook 'typed-clojure-mode)
;;   )


;;; [ helm-clojuredocs ] -- Searching for help in clojurdocs.org with helm.

;; (use-package helm-clojuredocs
;;   :ensure t
;;   :defer t
;;   :bind (:map clojure-mode-map
;;               ("C-h d d" . helm-clojuredocs))
;;   )


;;; [ elein ] -- running Leiningen commands from Emacs.

(use-package elein
  :ensure t)

;;; [ Boot ]

(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode)) ; recognize .boot file
(add-to-list 'magic-mode-alist '(".* boot" . clojure-mode)) ; recognize script file using shebang


;;; [ clomacs ] -- Clomacs simplifies call Clojure code from Emacs lisp.

(use-package clomacs
  :ensure t
  :defer t)


(provide 'init-my-prog-lang-clojure)

;;; init-my-prog-lang-clojure.el ends here
