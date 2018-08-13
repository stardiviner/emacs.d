;;; init-prog-lang-clojure.el --- init for Clojure.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ clojure-mode ]

(use-package clojure-mode
  :ensure t
  :ensure subword
  :ensure-system-package clojure
  :defer t
  :mode (
         ;; Boot files
         ("\\.boot\\'" . clojure-mode) ; recognize .boot file
         (".* boot" . clojure-mode) ; recognize script file using shebang
         )
  :config
  (setq cider-repl-display-help-banner nil) ; inhibit CIDER REPL help banner.
  (autoload 'my-lisp-common-settings "init-prog-lang-lisp.el")
  (add-hook 'clojure-mode-hook #'my-lisp-common-settings)
  
  (autoload 'my-lisp-repl-common-settings "init-prog-lang-lisp.el")
  (add-hook 'cider-repl-mode-hook #'my-lisp-repl-common-settings)

  ;; (add-hook 'clojure-mode-hook 'smartparens-strict-mode)
  ;; `subword-mode' is quite useful since we often have to deal with Java class
  ;; and method names.
  (add-hook 'clojure-mode-hook #'subword-mode)

  (setq clojure-align-forms-automatically t)
  
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
    (font-lock-add-keywords
     'clojure-mode `(("\\(#_\\)" ; #_
                      (0 (progn (compose-region (match-beginning 1)
                                                (match-end 1) "…")
                                nil)))))
    (font-lock-add-keywords
     'clojure-mode `(("\\(#\\)\"" ; #"regexp"
                      (0 (progn (compose-region (match-beginning 1)
                                                (match-end 1) "⋐")
                                nil)))))
    ;; `java.io.BufferedReader.' (class constructor)
    ;; (font-lock-add-keywords
    ;;  'clojure-mode `(("([^\\..*]*\\(\\.\\)\\{1\\}[\[:space:]\n)]"
    ;;                   (0 (progn (compose-region (match-beginning 1)
    ;;                                             (match-end 1) "⊿")
    ;;                             nil)))))
    )

  (use-package clojure-mode-extra-font-locking
    :ensure t)

  (use-package subword
    :ensure t
    :config
    (add-hook 'clojure-mode-hook 'subword-mode))
  )

;;; [ inf-clojure ] --

(use-package inf-clojure
  :ensure t
  :defer t
  :commands (inf-clojure)
  ;; :init (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)
  :config
  ;; fix `inf-clojure-minor-mode' conflict wiith `cider-imode' in Clojure buffer of `ob-clojure'.
  ;; (defun inf-clojure-disable-clojure (&optional arg)
  ;;   (cider-mode -1)
  ;;   (inf-clojure-minor-mode 1))
  ;; (advice-add 'org-edit-special :after 'inf-clojure-disable-clojure)
  ;; (add-hook 'inf-clojure-mode-hook #'subword-mode)
  
  ;; FIXME: it caused auto add newlines.
  ;; (add-hook 'inf-clojure-mode-hook #'eldoc-mode)
  ;; manage inf-clojure popup buffers.
  (add-to-list 'display-buffer-alist
               '("^\\*inf-clojure*\\*" (display-buffer-reuse-window display-buffer-below-selected)))
  )

;;; [ CIDER ] -- CIDER is a Clojure IDE and REPL for Emacs

(use-package cider
  :ensure t
  :defer t
  :after clojure-mode
  :bind (:map cider-doc-map
              ("c" . helm-cider-cheatsheet)
              ("C-c" . helm-cider-cheatsheet))
  :init (add-hook 'clojure-mode-hook #'cider-mode)
  ;; manage CIDER popup buffers.
  (add-to-list 'display-buffer-alist
               '("^\\*cider-.*\\*" (display-buffer-reuse-window display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist
               '("^\\*nrepl-.*\\*" (display-buffer-reuse-window display-buffer-below-selected)))
  :config
  ;; Clojure
  ;; (setq cider-default-repl-command "lein") ; TODO: change to use "clojure-clj" in future.
  ;; ClojureScript
  ;; (setq cider-default-cljs-repl "Figwheel") ; "Nashorn"

  (setq
   ;; resources
   ;; cider-prefer-local-resources t
   ;; font-lock
   cider-font-lock-dynamically '(macro core deprecated function)
   ;; indentation
   cider-dynamic-indentation nil
   ;; REPL
   cider-repl-result-prefix ";; => "
   )

  ;; annotations
  ;; remove "<>" from "<annontaion>"
  ;; (defun my/cider-default-annotate-completion-function (type ns)
  ;;   "Get completion function based on TYPE and NS."
  ;;   (concat (when ns (format " (%s)" ns))
  ;;           (when type (format " %s" type))))
  ;; (setq cider-annotate-completion-function #'my/cider-default-annotate-completion-function)
  ;;
  ;; (setq cider-completion-annotations-alist
  ;;       `(("function" ,(all-the-icons-faicon "gg" :height 0.9 :v-adjust -0.05))
  ;;         ("method" ,(all-the-icons-material "functions" :height 0.9 :v-adjust -0.05))
  ;;         ("static-method" ,(all-the-icons-faicon "circle-o-notch" :height 0.9 :v-adjust -0.05))
  ;;         ("field" ,(all-the-icons-faicon "hashtag" :height 0.9 :v-adjust -0.05))
  ;;         ("class" ,(all-the-icons-faicon "puzzle-piece" :face 'company-tooltip-annotation :height 0.9 :v-adjust -0.05))
  ;;         ("keyword" ,(all-the-icons-faicon "heartbeat" :face 'company-tooltip-annotation :height 0.9 :v-adjust -0.05))
  ;;         ("local" ,(all-the-icons-faicon "circle-thin" :height 0.9 :v-adjust -0.05))
  ;;         ("var" ,(all-the-icons-faicon "fire" :height 0.9 :v-adjust -0.05))
  ;;         ("macro" ,(all-the-icons-faicon "codepen" :face 'company-tooltip-annotation :height 0.9 :v-adjust -0.05))
  ;;         ("namespace" ,(all-the-icons-faicon "cubes" :face 'company-tooltip-annotation :height 0.9 :v-adjust -0.05))
  ;;         ("protocol" ,(all-the-icons-faicon "link" :face 'company-tooltip-annotation :height 0.9 :v-adjust -0.05))
  ;;         ("record" ,(all-the-icons-faicon "list-alt" :face 'company-tooltip-annotation :height 0.9 :v-adjust -0.05))
  ;;         ("special-form" ,(all-the-icons-faicon "cog" :height 0.9 :v-adjust -0.05))
  ;;         ("type" ,(all-the-icons-faicon "dot-circle-o" :height 0.9 :v-adjust -0.05))
  ;;         ("import" ,(all-the-icons-octicon "package" :height 0.9 :v-adjust -0.05))
  ;;         ))

  ;; Enlighten faces
  ;; `cider-enlighten-mode' will extremely slow down Clojure/CIDER evaluation.
  ;; (add-hook 'cider-mode-hook #'cider-enlighten-mode)

  ;; auto completion with company-mode support
  ;; `cider-complete-at-point' in `completion-at-point-functions'
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)

  ;; (use-package cider-hydra
  ;;   :ensure t
  ;;   :config
  ;;   (add-hook 'cider-mode-hook #'cider-hydra-mode)
  ;;   (add-hook 'cider-repl-mode-hook #'cider-hydra-mode))
  
  ;; enable `eldoc' in relevant buffers.
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  ;; (add-hook 'cider-repl-mode-hook #'cider-repl-require-repl-utils) ; require common functions like doc, source, etc.
  (setq cider-eldoc-display-for-symbol-at-point t
        cider-eldoc-display-context-dependent-info t ; for datomic query input parameters
        )

  (add-hook 'cider-repl-mode-hook #'subword-mode)
  
  ;; auto inject Clojure dependencies.
  ;; (cider-add-to-alist 'cider-jack-in-dependencies "org.clojure/tools.nrepl" "0.2.13")
  ;; (add-to-list 'cider-jack-in-dependencies '("incanter" "1.9.2"))
  ;; (add-to-list 'cider-jack-in-dependencies '("org.clojure/core.async" "0.4.474"))
  (add-to-list 'cider-jack-in-dependencies '("criterium" "0.4.4"))
  ;; Check out function `cljr--inject-jack-in-dependencies'.
  
  ;; notify user CIDER is connected.
  ;; (add-hook 'cider-connected-hook
  ;;           (lambda ()
  ;;             (notifications-notify :title "CIDER connected"
  ;;                                   :body "CIDER process connected.")))

  ;; [ cider-profile ] [C-c C-=]
  
  ;; CIDER inspect command keybindings
  (unless (boundp 'cider-inspect-prefix)
    (define-prefix-command 'cider-inspect-prefix))
  (defun my:cider-setup-inspect-keybindings ()
    (local-set-key (kbd "C-c i") 'cider-inspect-prefix)
    (define-key cider-inspect-prefix (kbd "r") 'cider-inspect-last-result)
    (define-key cider-inspect-prefix (kbd "E") 'cider-inspect-expr)
    (define-key cider-inspect-prefix (kbd "d") 'cider-inspect-defun-at-point)
    (define-key cider-inspect-prefix (kbd "e") 'cider-inspect-last-sexp)
    (define-key cider-inspect-prefix (kbd "i") 'cider-inspect-read-and-inspect))
  (add-hook 'clojure-mode-hook #'my:cider-setup-inspect-keybindings)
  
  (defconst cider-metadata-buffer "*cider-metadata*")
  (defun cider-metadata (var &optional ns)
    "Show VAR's metadata in a separate buffer.
Optional argument NS, if not provided, defaults to
`cider-current-ns'."
    (interactive (list (cider-symbol-at-point)))
    (let* ((buf (current-buffer))
           (result-buffer (cider-popup-buffer cider-metadata-buffer t 'clojure-mode t))
           (handler (cider-popup-eval-out-handler result-buffer)))
      (with-current-buffer buf
        (cider-interactive-eval
         (format "(meta (var %s))" (concat (or ns (cider-current-ns)) "/" var))
         handler
         nil
         (cider--nrepl-pprint-request-plist (cider--pretty-print-width))))))

  ;; [ helm-cider ] -- Helm interface to CIDER.
  (use-package helm-cider
    :ensure t
    :init (add-hook 'cider-mode-hook #'helm-cider-mode))

  ;; bind keybindings to some not-bind wrapping functions in clojure-mode locally.
  (add-hook 'clojure-mode-hook
            (lambda ()
              (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
              (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly))
            nil 'local)

  ;; REPL history
  (define-key cider-repl-mode-map (kbd "M-r") 'cider-repl-history)

  ;; CIDER helper functions
  (defun my/cider-repl-eval (input)
    "Execute Clojure `INPUT' in CIDER REPL.

Usage: (my/cider-repl-eval \"\(clojure expr\)\")"
    (notifications-notify
     :title "CIDER nREPL evaluation starting."
     :body input)
    (cider-interactive-eval input
                            #'(lambda (event)
                                (notifications-notify
                                 :title "CIDER form evaluation finished.")))
    (sit-for 5))
  
  ;; (add-hook 'cider-connected-hook
  ;;           #'(lambda ()
  ;;               (sit-for 10)
  ;;               ;; import Java object static methods like `.getCanonicalFile'.
  ;;               (my/cider-repl-eval "(import [java.io File InputStream])")
  ;;               ;; load Incanter commonly usaed namespaces.
  ;;               ;; (my/cider-repl-eval "(use '(incanter core stats datasets charts io pdf))")
  ;;               (my/cider-repl-eval "(use '(incanter stats charts))")
  ;;               ) t)
  )

;;; run test when load file.

;; (defun cider-tdd-test ()
;;   "Thin wrapper around `cider-test-run-tests'."
;;   (when (cider-connected-p)
;;     (let ((cider-auto-select-test-report-buffer nil)
;;           (cider-test-show-report-on-success nil))
;;       (cider-test-run-ns-tests nil)
;;       (message "CIDER TDD test run successfully."))))
;;
;; (define-minor-mode cider-tdd-mode
;;   "Run all tests whenever a file is saved."
;;   t nil nil
;;   :global t
;;   (if cider-tdd-mode
;;       (add-hook 'cider-file-loaded-hook #'cider-tdd-test)
;;     (remove-hook 'cider-file-loaded-hook #'cider-tdd-test)))
;;
;; (add-hook 'clojure-mode-hook 'cider-tdd-mode)

;;; [ debux.el ] -- Integrate Clojure/ClojureScript debugger "Debux" into Emacs.

;; TODO: https://github.com/philoskim/debux
;; (use-package debux
;;   :load-path "~/Code/Emacs/debux/"
;;   ;; :config
;;   )

;;; [ flycheck-clojure, squiggly-clojure ] --

;; (use-package flycheck-clojure
;;   :ensure t
;;   :defer t
;;   :after flycheck
;;   :init (flycheck-clojure-setup))

;;; [ clj-refactor ]

(use-package clj-refactor
  :ensure t
  ;; :pin "melpa-unstable" ; fix clj-refactor not compatible with CIDER issue.
  :defer t
  :init
  (defun my:clj-refactor-setup ()
    (clj-refactor-mode 1)
    ;; (cljr-add-keybindings-with-prefix "C-c C-m")
    (define-key clj-refactor-map (kbd "C-c C-m") #'hydra-cljr-help-menu/body))
  (add-hook 'clojure-mode-hook #'my:clj-refactor-setup)
  (add-hook 'cider-repl-mode-hook #'my:clj-refactor-setup)
  ;; :config
  ;; skip Tab in `cljr-add-require-to-ns' snippet.
  ;; (advice-add 'cljr-add-require-to-ns :after
  ;;             (lambda (&rest _)
  ;;               (yas-next-field)
  ;;               (yas-next-field)))

  ;; FIXME: temporary solution for clj-refactor.
  (with-eval-after-load "clj-refactor"
    (remove-hook 'find-file-hook #'cljr--ensure-no-dashes-in-filename))
  
  (add-to-list 'display-buffer-alist
               '("^\\*cljr-*\\*" . (display-buffer-below-selected)))
  )

;;; [ kibit-helper ] -- Conveniently use the Kibit Leiningen plugin from Emacs.

;; (use-package kibit-helper
;;   :ensure t
;;   :defer t
;;   :commands (kibit kibit-current-file kibit-accept-proposed-change)
;;   :bind (:map clojure-mode-map
;;               ("C-x C-`" . kibit-accept-proposed-change)))

;;; [ cider-decompile ] -- decompilation extension for cider.

;; (use-package cider-decompile
;;   :ensure t
;;   :defer t
;;   :commands (cider-decompile-func cider-decompile-ns-func))

;;; [ cider-spy ] -- Spy on CIDER to get Info.

;; (use-package cider-spy
;;   :ensure t
;;   :defer t)

;;; [ ob-clojure ]
;;; Org-mode Babel Clojure
(require 'ob-clojure)

(add-to-list 'org-babel-load-languages '(clojure . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))

(setq org-babel-clojure-backend 'cider)

;; whether check Org-mode buffer's (ns ) declarations.
;; (setq cider-auto-track-ns-form-changes t)

;; let `ob-clojure' babel src blocks allow evaluation.
(add-to-list 'org-babel-default-header-args:clojure
             '(:eval . "yes"))
(add-to-list 'org-babel-default-header-args:clojure
             '(:noweb . "yes"))
;; (add-to-list 'org-babel-default-header-args:clojure
;;              '(:results . "pp"))

;; No timeout when executing calls on Cider via nrepl
(setq org-babel-clojure-sync-nrepl-timeout 30)
(add-to-list 'org-babel-default-header-args:clojure ; for Clojure `dotimes' etc.
             '(:show-process . "no"))


;;; [ ob-clojure-literate ] -- Clojure Literate Programming in Org-mode Babel.

(require 'ob-clojure-literate)
(setq ob-clojure-literate-auto-jackin-p t)
(setq ob-clojure-literate-project-location "~/.emacs.d/Org-mode/")
(define-key org-babel-map (kbd "M-c") 'ob-clojure-literate-mode)

;;; [ ob-clojurescript ] -- org-babel support for ClojureScript

(use-package ob-clojurescript
  :ensure t
  :defer t
  :init
  (require 'ob-clojurescript)
  (add-to-list 'org-babel-load-languages '(clojurescript . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

;;; [ typed-clojure-mode ] -- Typed Clojure minor mode for Emacs.

;; (use-package typed-clojure-mode
;;   :ensure t
;;   :defer t
;;   :init
;;   (add-hook 'clojure-mode-hook 'typed-clojure-mode)
;;   )

;;; [ helm-clojuredocs ] -- Searching for help in clojurdocs.org with Helm.

(use-package helm-clojuredocs
  :ensure t
  :after cider
  :bind (:map cider-doc-map ("M-d" . helm-clojuredocs)))

;;; [ elein ] -- running Leiningen commands from Emacs.

(use-package elein
  :ensure t
  :ensure-system-package (lein . "wget 'https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein' && mv lein ~/bin/ && chmod 755 ~/bin/lein")
  :defer t
  :init
  (defun elein-lein-try ()
    (interactive)
    (if (equal "*scratch*" (buffer-name))
        (progn
          (setq-local inferior-lisp-program "lein try tentacles")
          (command-execute 'inferior-lisp)
          )))
  )

;;; [ clomacs ] -- Clomacs simplifies call Clojure code from Emacs lisp.

(use-package clomacs
  :ensure t
  :defer t)

;;; [ cljsbuild-mode ] -- A minor mode for the ClojureScript 'lein cljsbuild' command.

(use-package cljsbuild-mode
  :ensure t
  :defer t)

;;; [ 4clojure ] -- Open and evaluate 4clojure.com questions in Emacs.

(use-package 4clojure
  :ensure t
  :defer t
  :config
  (defadvice 4clojure-open-question (around 4clojure-open-question-around)
    "Start a cider/nREPL connection if one hasn't already been started when
opening 4clojure questions"
    ad-do-it
    (unless cider-current-clojure-buffer
      (cider-jack-in)))

  (defun my/4clojure-check-and-proceed ()
    "Check the answer and show the next question if it worked."
    (interactive)
    (let ((result (4clojure-check-answers)))
      (unless (string-match "failed." result)
        (4clojure-next-question))))
  (define-key clojure-mode-map (kbd "C-c C-c") 'my/4clojure-check-and-proceed)
  )

;;; [ YeSQL Ghosts ] -- Display ghostly YeSQL defqueries inline, in Emacs.

(use-package yesql-ghosts
  :ensure t
  :defer t
  :config
  (add-hook 'cider-mode-hook 'yesql-ghosts-auto-show-ghosts)
  (setq yesql-ghosts-show-ghosts-automatically t
        yesql-ghosts-show-descriptions t)
  )

;;; [ HugSQL Ghosts ] -- Display ghostly HugSQL defqueries inline, in Emacs.

(use-package hugsql-ghosts
  :ensure t
  :defer t
  :config
  (add-hook 'cider-mode-hook 'hugsql-ghosts-install-hook)
  (setq hugsql-ghosts-newline-before-docstrings t)
  )

;;; [ parseclj ] -- EDN reader and Clojure Parser for Emacs Lisp

(use-package parseclj
  :ensure t
  :defer t)



(provide 'init-prog-lang-clojure)

;;; init-prog-lang-clojure.el ends here
