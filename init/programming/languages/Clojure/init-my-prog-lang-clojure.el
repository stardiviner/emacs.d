;;; init-my-prog-lang-clojure.el ---
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ clojure-mode ]

(use-package clojure-mode
  :ensure t
  :config
  ;; `subword-mode' is quite useful since we often have to deal with Java class
  ;; and method names.
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'eldoc-mode)
  ;; (add-hook 'clojure-mode-hook 'smartparens-strict-mode)
  )


;;; [ inf-clojure ] -- basic interaction with a Clojure subprocess

;;; Usage:
;;
;; - [M-x inf-clojure/run-clojure] / [C-c C-z]
;; - [M-j] :: new line in sexp.

;; (use-package inf-clojure
;;   :ensure t
;;   :config
;;   (add-hook 'inf-clojure-mode-hook #'subword-mode)
;;   (add-hook 'inf-clojure-mode-hook #'eldoc-mode)
;;   (define-key my-prog-inferior-map (kbd "C") 'inf-clojure)
;;
;;   (add-hook 'inf-clojure-mode-hook #'cider-mode)
;;   )


;;; [ CIDER ] -- CIDER is a Clojure IDE and REPL for Emacs

;;; Usage:
;;
;; - workflow:
;;   1. [M-x cider-jack-in]
;;   2. (optional) [M-x cider-connect]
;;   3. [C-c C-l] :: `cider-load-file'

(use-package cider
  :ensure t
  :config
  (setq cider-auto-mode t
        nrepl-hide-special-buffers t
        cider-auto-select-error-buffer t
        nrepl-buffer-name-separator " "
        nrepl-buffer-name-show-port nil

        ;; resources
        cider-prefer-local-resources t

        ;; font-lock
        cider-font-lock-dynamically '(macro core deprecated function)

        ;; indent
        cider-dynamic-indentation t
        
        ;; REPL
        ;; cider-inject-dependencies-at-jack-in t
        cider-repl-display-in-current-window nil
        cider-repl-pop-to-buffer-on-connect nil
        cider-prompt-save-file-on-load t
        cider-repl-result-prefix ";; => "
        cider-interactive-eval-result-prefix ";; => "
        cider-repl-use-clojure-font-lock t
        ;; cider-repl-tab-command 'cider-repl-indent-and-complete-symbol
        ;; cider-known-endpoints '(("host-a" "10.10.10.1" "7888") ("host-b" "7888"))
        cider-repl-use-pretty-printing t
        cider-repl-wrap-history t
        cider-repl-history-size 500
        cider-repl-history-file nil
        cider-show-error-buffer 'only-in-repl

        ;; pretty-printing
        cider-pprint-fn 'fipp
        
        ;; Eval
        cider-show-eval-spinner t
        cider-use-overlays 'both
        cider-overlays-use-font-lock t ; use overlay for results.
        cider-result-use-clojure-font-lock t
        cider-eval-result-duration nil

        ;; Enlighten
        
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
        )

  ;; Complete & annotations
  (setq cider-completion-use-context t
        cider-annotate-completion-candidates t
        ;; cider-annotate-completion-function
        cider-completion-annotations-include-ns 'always ; 'unqualified
        cider-completion-annotations-alist '(("class" "c")
                                             ("field" "fi")
                                             ("function" "λ") ; f,
                                             ("import" "i")
                                             ("keyword" "k")
                                             ("local" "l")
                                             ("macro" "♩")
                                             ("method" "m") ; me,
                                             ("namespace" "n")
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
  (add-hook 'clojure-mode-hook 'cider-enlighten-mode)
  
  (set-face-attribute 'cider-enlightened nil
                      :background "black" :foreground "yellow"
                      )
  (set-face-attribute 'cider-enlightened-local nil
                      :background "black" :foreground "orange"
                      )

  ;; eval sexp result overlays
  (set-face-attribute 'cider-result-overlay-face nil
                      :foreground "orange"
                      :background (color-darken-name (face-background 'default) 3)
                      :box '(:line-width -1 :color "dim gray")
                      )

  ;; enable `cider-mode' in `clojure-mode'.
  (add-hook 'clojure-mode-hook 'cider-mode)
  
  ;; auto completion with company-mode support
  ;; `cider-complete-at-point' in `completion-at-point-functions'
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)

  ;; enable `eldoc' in relevant buffers.
  (add-hook 'cider-mode-hook #'eldoc-mode)

  (add-hook 'cider-repl-mode-hook #'subword-mode)

  ;; notify user CIDER is connected.
  (add-hook 'cider-connected-hook
            '(lambda ()
               (notifications-notify :title "CIDER connected"
                                     :body "CIDER process connected."))
            )
  
  ;; switch to cider-repl buffer.
  (defun my-cider-switch-to-repl-buffer ()
    (interactive)
    (let ((cider-repl "*cider-repl localhost*")
          (cider-command "cider-jack-in"))
      (if (get-buffer cider-repl)
          (switch-to-buffer cider-repl)
        (message "CIDER REPL buffer not available. starting a new one...")
        (cider-jack-in)
        )))

  ;; (define-key clojure-mode-map [remap cider-switch-to-repl-buffer]
  ;;   'my-cider-switch-to-repl-buffer)
  (define-key my-prog-inferior-map (kbd "c") 'my-cider-switch-to-repl-buffer)
  )



;;; [ cider-decompile ]

;;; Usage:
;;
;; - [M-x cider-decompile-func [RET] main [RET]]
;; - [M-x cider-decompile-ns-func [RET] myotherns.core/other-main [RET]]

(use-package cider-decompile
  ;; :ensure t
  ;; :config
  ;; (define-key clojure-mode-map (kbd "??") cider-decompile-func)
  ;; (define-key clojure-mode-map (kbd "??") cider-decompile-ns-func)
  )


;;; [ cider-spy ] -- Spy on CIDER to get Info.

;; (use-package cider-spy)


;;; [ cider-profile ]

;; (use-package cider-profile)


;;; [ flycheck-clojure, squiggly-clojure ] --

(use-package flycheck-clojure
  :ensure t
  :config
  (eval-after-load 'flycheck
    '(flycheck-clojure-setup))
  )


;;; [ clj-refactor ]

;;; Usage:
;;
;; - [C-c RET] :: prefix.
;; - [C-c RET C-h] :: check the list of functions.
;;
;; All functions in clj-refactor have a two-letter mnemonic shortcut. For
;; instance, rename-file-or-dir is `rf'. You get to choose how those are bound.
;; e.g. [C-c RET rf] -- rename-file
;;
;;  
;;  ad :: add declaration for current top-level form
;;  ai :: add import to namespace declaration, then jump back
;;  ar :: add require to namespace declaration, then jump back (see optional setup)
;;  au :: add "use" (ie require refer all) to namespace declaration, then jump back
;;  cc :: cycle surrounding collection type
;;  ci :: cycle between if and if-not
;;  cp :: cycle privacy of defns and defs
;;  dk :: destructure keys
;;  el :: expand let
;;  fe :: create function stub from example usage
;;  il :: introduce let
;;  mf :: move one or more forms to another namespace, :refer any functions
;;  ml :: move to let
;;  pc :: run project cleaner functions on the whole project
;;  pf :: promote function literal or fn, or fn to defn
;;  rf :: rename file or directory and update all affected files
;;  rl :: remove-let, inline all variables and remove the let form
;;  rr :: remove unused requires
;;  ru :: replace all :use in namespace with :refer :all
;;  sc :: show the project's changelog to learn about recent changes
;;  sn :: sort :use, :require and :import in the ns form
;;  sp :: Sort all dependency vectors in project.clj
;;  sr :: stop referring (removes :refer [] from current require, fixing references)
;;  tf :: wrap in thread-first (->) and fully thread
;;  th :: thread another expression
;;  tl :: wrap in thread-last (->>) and fully thread
;;  ua :: fully unwind a threaded expression
;;  uw :: unwind a threaded expression
;;
;;
;; The following functions are available, and supported, but used rarely enough
;; that they're not given a keybinding:
;;
;; `cljr-reify-to-record' change a call to reify with a call to a defrecord constructor.
;;
;; Using `refactor-nrepl', you also get:
;;
;;  am :: add a missing libspec
;;  as :: add implementation stubs for an interface or protocol
;;  ap :: add a dependency to your project
;;  cn :: Perform various cleanups on the ns form
;;  ef :: Extract function
;;  fu :: Find usages
;;  hd :: Hotload dependency
;;  is :: Inline symbol
;;  rd :: Remove (debug) function invocations
;;  rs :: Rename symbol
;;  
;;  Combine with your keybinding prefix/modifier.
;;
;; Custom
;;
;; - [customize-group cljr]
;;
;; Reload config
;;
;; You can use `cljr-reload-config' to resubmit configuration settings to
;; `refactor-nrepl' after changing them in emacs. This is a good alternative to
;; restarting the repl whenever you change settings affecting the middleware.


;; (use-package clj-refactor
;;   :ensure t
;;   :config
;;   (add-hook 'clojure-mode-hook (lambda ()
;;                                  (clj-refactor-mode 1)
;;                                  ;; insert keybinding setup here
;;                                  (cljr-add-keybindings-with-prefix "C-c RET")
;;                                  ))
;;
;;   ;; no auto sort
;;   (setq cljr-auto-sort-ns nil)
;;
;;   ;; do not prefer prefixes when using clean-ns
;;   (setq cljr-favor-prefix-notation nil)
;;   )


;;; [ cider-eval-sexp-fu ]

(use-package cider-eval-sexp-fu
  :ensure t)


;;; [ align-cljlet ]

(use-package align-cljlet
  ;; :ensure t
  ;; :config
  ;; (define-key clojure-mode-map (kbd "??") 'align-cljlet)
  )


;;; [ typed-clojure-mode ] -- 
;;
;;; Usage:
;;
;;| Keyboard Shortcut | Description                                 | Command                  |
;;|-------------------+---------------------------------------------+--------------------------|
;;| C-c C-x n         | Print errors in the namespace               | typed-clojure-check-ns   |
;;| C-c C-x f         | Check the preceding form or symbol          | typed-clojure-check-form |
;;| C-c C-a v         | Insert (ann ... ) above the top expression  | typed-clojure-ann-var    |
;;| C-c C-a f         | Wrap the current form with (ann-form ... t) | typed-clojure-ann-form   |

(use-package typed-clojure-mode
  ;; :ensure t
  ;; :config
  ;; (add-hook 'clojure-mode-hook 'typed-clojure-mode)
  )


;;; [ helm-clojuredocs ] -- Searching for help in clojurdocs.org with helm.

;; (use-package helm-clojuredocs
;;   :ensure t
;;   :config
;;   (define-key clojure-mode-map (kbd "C-h d d") 'helm-clojuredocs)
;;   )


;;; [ clojure-cheatsheet ]

;; https://github.com/clojure-emacs/clojure-cheatsheet


(provide 'init-my-prog-lang-clojure)

;;; init-my-prog-lang-clojure.el ends here
