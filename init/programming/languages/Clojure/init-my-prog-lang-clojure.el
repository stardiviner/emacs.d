;;; init-my-prog-lang-clojure.el --- init for Clojure.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ clojure-mode ]

(use-package clojure-mode
  :ensure t
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
    )
  )


;;; [ CIDER ] -- CIDER is a Clojure IDE and REPL for Emacs

(use-package cider
  :ensure t
  :config
  (setq cider-auto-mode t
        nrepl-hide-special-buffers nil
        cider-auto-select-error-buffer t
        nrepl-buffer-name-separator " "
        nrepl-buffer-name-show-port nil
        nrepl-log-messages t
        nrepl-prompt-to-kill-server-buffer-on-quit t

        ;; versions
        cider-latest-clojure-version "1.9.0-alpha12"
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
        ;; cider-preferred-build-tool "lein"
        )

  ;; Complete & annotations
  (setq cider-completion-use-context t
        cider-annotate-completion-candidates t
        ;; cider-completion-annotations-include-ns 'always ; 'unqualified
        cider-completion-annotations-alist '(("class" "c")
                                             ("field" "fi")
                                             ("function" "λ") ; f, λ
                                             ("import" "i")
                                             ("keyword" "k")
                                             ("local" "l")
                                             ("macro" "♪") ; ♪, ➜
                                             ("method" "me")
                                             ("namespace" "ns")
                                             ("protocol" "p")
                                             ("protocol-function" "pf")
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
                      :family "Comic Neue"
                      )

  ;; enable `cider-mode' in `clojure-mode'.
  (add-hook 'clojure-mode-hook #'cider-mode)
  
  ;; auto completion with company-mode support
  ;; `cider-complete-at-point' in `completion-at-point-functions'
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)

  ;; enable `eldoc' in relevant buffers.
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (setq cider-eldoc-display-for-symbol-at-point t ; NOTE: enable this will cause high CPU.
        cider-eldoc-max-class-names-to-display 3)

  (add-hook 'cider-repl-mode-hook #'subword-mode)

  
  ;; auto start CIDER jack-in.
  (add-hook 'after-init-hook #'cider-jack-in)

  ;; notify user CIDER is connected.
  (add-hook 'cider-connected-hook
            (lambda ()
              (notifications-notify :title "CIDER connected"
                                    :body "CIDER process connected."))
            )

  
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
        ;; TODO: set variable `cider-cljs-lein-repl'.
        (cider-jack-in-clojurescript))))

  (define-key clojure-mode-map (kbd "C-c C-s") 'my-cider-clojure-repl-switch)
  (define-key clojurescript-mode-map (kbd "C-c C-s") 'my-cider-cljs-repl-switch)
  (define-key my-inferior-lisp-map (kbd "c") 'my-cider-clojure-repl-switch)
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

(set-face-attribute 'cider-test-success-face nil
                    :foreground "green" :background nil)

;; (add-hook 'clojure-mode-hook 'cider-tdd-mode)


;;; [ cider-decompile ]

;;; Usage:
;;
;; - [M-x cider-decompile-func [RET] main [RET]]
;; - [M-x cider-decompile-ns-func [RET] myotherns.core/other-main [RET]]

;; (use-package cider-decompile
;;   :ensure t
;;   ;; :config
;;   ;; (define-key clojure-mode-map (kbd "??") cider-decompile-func)
;;   ;; (define-key clojure-mode-map (kbd "??") cider-decompile-ns-func)
;;   )


;;; [ cider-spy ] -- Spy on CIDER to get Info.

;; (use-package cider-spy
;;   :ensure t)


;;; [ cider-profile ]

;; (use-package cider-profile
;;   :ensure t
;;   :config
;;   (add-hook 'cider-mode-hook 'cider-profile-mode)
;;   (add-hook 'cider-repl-mode-hook 'cider-profile-mode)
;;   )


;;; [ flycheck-clojure, squiggly-clojure ] --

(use-package flycheck-clojure
  :ensure t
  :config
  (with-eval-after-load 'flycheck
    (flycheck-clojure-setup))
  )


;;; [ clj-refactor ]

(use-package clj-refactor
  :ensure t
  :config
  ;; (setq cljr-warn-on-eval nil)
  
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode 1)
              ;; insert keybinding setup here
              (cljr-add-keybindings-with-prefix "M-RET")
              ))

  ;; no auto sort
  (setq cljr-auto-sort-ns nil)

  ;; do not prefer prefixes when using clean-ns
  (setq cljr-favor-prefix-notation nil)

  ;; skip Tab in `cljr-add-require-to-ns' snippet.
  ;; (advice-add 'cljr-add-require-to-ns :after
  ;;             (lambda (&rest _)
  ;;               (yas-next-field)
  ;;               (yas-next-field)))
  )


;;; [ cider-eval-sexp-fu ]

(use-package cider-eval-sexp-fu
  :ensure t)


;;; [ align-cljlet ]

(use-package align-cljlet
  ;; :ensure t
  ;; :config
  ;; (define-key clojure-mode-map (kbd "??") 'align-cljlet)
  )


;;; [ typed-clojure-mode ] -- Typed Clojure minor mode for Emacs.

;; (use-package typed-clojure-mode
;;   :ensure t
;;   :config
;;   (add-hook 'clojure-mode-hook 'typed-clojure-mode)
;;   )


;;; [ helm-clojuredocs ] -- Searching for help in clojurdocs.org with helm.

;; (use-package helm-clojuredocs
;;   :ensure t
;;   :config
;;   (define-key clojure-mode-map (kbd "C-h d d") 'helm-clojuredocs)
;;   )


;;; [ elein ] -- running Leiningen commands from Emacs.

(use-package elein
  :ensure t)


;;; [ clojars ] -- Emacs Interface to Clojars.org

(use-package clojars
  :ensure t)


;;; [ clomacs ] -- Clomacs simplifies call Clojure code from Emacs lisp.

(use-package clomacs
  :ensure t)


(provide 'init-my-prog-lang-clojure)

;;; init-my-prog-lang-clojure.el ends here
