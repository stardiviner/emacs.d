;;; init-prog-lang-clojure.el --- init for Clojure.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ clojure-mode ]

(use-package clojure-mode ; [C-c C-r] prefix
  :ensure t
  :defer t
  :commands (clojure-add-arity)
  :custom ((clojure-align-forms-automatically t))
  :mode (("\\.\\(clj\\|dtm\\|edn\\)\\'" . clojure-mode)
         ("\\(?:build\\|profile\\)\\.boot\\'" . clojure-mode))
  :hook ((clojure-mode . paredit-mode)
         (clojure-mode . hl-sexp-mode)
         (clojure-mode . rainbow-delimiters-mode))
  :config
  (defun clojure-mode-add-electric-pair-for-inline-code ()
    (add-to-list 'electric-pair-pairs '(?\` . ?\`)))
  (add-hook 'clojure-mode-hook #'clojure-mode-add-electric-pair-for-inline-code)

  ;; (add-hook 'clojure-mode-hook 'smartparens-strict-mode)

  ;; treat `foo-bar' or `:baz' as a symbol.
  (defun clojure-mode-extend-symbol-syntax ()
    (dolist (c (string-to-list ":_-?!#*"))
      (modify-syntax-entry c "w" clojure-mode-syntax-table)))
  (add-hook 'clojure-mode-hook #'clojure-mode-extend-symbol-syntax)

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

  ;; make the [M-;] and `banner-comment' works correct.
  (defun clojure-mode-set-comment-char ()
    (setq-local comment-start ";;"))
  (add-hook 'clojure-mode-hook #'clojure-mode-set-comment-char)
  )

(use-package clojure-mode-extra-font-locking
  :ensure t
  :after clojure-mode)

;; `subword-mode' is quite useful since we often have to deal with Java class
;; and method names.
(use-package subword
  :ensure t
  :defer t
  :hook ((clojure-mode . subword-mode)
         (cider-repl-mode . subword-mode)))

;;; [ inf-clojure ] -- Run an external Clojure process in an Emacs buffer.

(use-package inf-clojure
  :ensure t
  :defer t
  :commands (inf-clojure)
  :hook ((inf-clojure-mode . eldoc-mode)
         (inf-clojure-mode . subword-mode)
         ;; (clojure-mode . inf-clojure-minor-mode) ; NOTE: `inf-clojure-minor-mode' will add `inf-clojure-completion-at-point' to `completion-at-point-functions' (capf).
         )
  :init (add-to-list 'display-buffer-alist '("^\\*inf-clojure*\\*" . (display-buffer-below-selected)))
  ;; :config
  ;; fix `inf-clojure-minor-mode' conflict wiith `cider-imode' in Clojure buffer of `ob-clojure'.
  ;; (defun inf-clojure-disable-clojure (&optional arg)
  ;;   (cider-mode -1)
  ;;   (inf-clojure-minor-mode 1))
  ;; (advice-add 'org-edit-special :after 'inf-clojure-disable-clojure)
  )

;;; [ CIDER ] -- CIDER is a Clojure IDE and REPL for Emacs

(use-package cider
  :ensure t
  :defer t
  :after clojure-mode
  :commands (cider-jack-in)
  :custom ((cider-font-lock-dynamically '(macro core deprecated function))
           (cider-repl-result-prefix ";; => ")
           (cider-eldoc-display-context-dependent-info t))
  :hook ((cider-repl-mode . paredit-mode)
         (cider-repl-mode . hl-sexp-mode)
         (cider-repl-mode . rainbow-delimiters-mode)
         ;; auto completion with company-mode support
         ;; `cider-complete-at-point' in `completion-at-point-functions'
         (cider-repl-mode . company-mode)
         (cider-mode . company-mode)
         ;; enable `eldoc' in relevant buffers.
         (cider-mode . eldoc-mode)
         (cider-repl-mode . eldoc-mode)
         ;; support CIDER fuzzy completion style like: `n-m', `j.l.S' etc.
         ;; (cider-repl-mode . cider-company-enable-fuzzy-completion)
         ;; (cider-mode . cider-company-enable-fuzzy-completion)
         )
  :init
  ;; manage CIDER popup buffers.
  (add-to-list 'display-buffer-alist '("^\\*cider-.*\\*" . (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist '("^\\*nrepl-.*\\*" . (display-buffer-below-selected)))
  ;; (use-package cider-hydra
  ;;   :ensure t
  ;;   :hook ((cider-mode . cider-hydra-mode)
  ;;          (cider-repl-mode . cider-hydra-mode)))
  :config
  ;; Frequently used connections
  ;; TODO: need to know how to run Docker container REPL with specific port.
  ;; $ docker run --name clojure -p :7788 -it clojure:tools-deps clj
  ;; (setq cider-known-endpoints '(("docker-clojure" "127.0.0.1" "7888")))

  ;; Enlighten faces
  ;; NOTE `cider-enlighten-mode' will extremely slow down Clojure/CIDER evaluation.
  ;; NOTE `cider-enlighten-mode' will cause Org Mode source block evaluate filter error when you eval in opened source block temp buffer.
  ;; (defun my/cider-enlighten-mode-enable ()
  ;;   "Enable `cider-enlighten-mode' only when in real Clojure
  ;;   source code file instead of virtual temprary buffer like Org
  ;;   Babel opened buffers."
  ;;   (unless (buffer-file-name)
  ;;     (cider-enlighten-mode)))
  ;; (add-hook 'cider-mode-hook #'my/cider-enlighten-mode-enable)
  
  ;; [M-.] jump to symbol definition]
  (add-to-list 'cider-jdk-src-paths '"/usr/lib/jvm/default/lib/src.zip" :append)
  
  ;; (setq cider-jump-to-pop-to-buffer-actions '((display-buffer-reuse-window (window-height . 0.3))))
  
  ;; auto inject Clojure dependencies.
  ;; (cider-add-to-alist 'cider-jack-in-dependencies "org.clojure/tools.nrepl" "0.2.13")
  ;; hot load dependencies.
  ;; (add-to-list 'cider-jack-in-dependencies '("com.cemerick/pomegranate" "1.1.0"))
  (add-to-list 'cider-jack-in-dependencies '("hashp/hashp" "0.2.0"))
  (add-to-list 'cider-jack-in-dependencies '("datawalk/datawalk" "0.1.12"))
  (add-to-list 'cider-jack-in-dependencies '("io.github.erdos/erdos.assert" "0.1.0"))
  (add-to-list 'cider-jack-in-dependencies '("criterium/criterium" "0.4.5"))
  ;; auto add incanter as dependency for Org Mode clojure Babel generate plot image result.
  ;; (add-to-list 'cider-jack-in-dependencies '("incanter" "1.9.2"))
  (add-to-list 'cider-jack-in-dependencies '("net.mikera/core.matrix" "0.62.0"))
  ;; Check out function `cljr--inject-jack-in-dependencies'.

  ;; [ cider-profile ] [C-c C-=]
  
  ;; CIDER inspect command keybindings [C-c M-i] `cider-inspect'.
  (unless (boundp 'cider-inspect-prefix)
    (define-prefix-command 'cider-inspect-prefix))
  (defun my:cider-setup-inspect-keybindings ()
    (local-set-key (kbd "C-c i") 'cider-inspect-prefix)
    (define-key cider-inspect-prefix (kbd "r") 'cider-inspect-last-result)
    (define-key cider-inspect-prefix (kbd "E") 'cider-inspect-expr)
    (define-key cider-inspect-prefix (kbd "d") 'cider-inspect-defun-at-point)
    (define-key cider-inspect-prefix (kbd "e") 'cider-inspect-last-sexp))
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
  (define-key cider-inspect-prefix (kbd "m") 'cider-metadata)

  ;; bind keybindings to some not-bind wrapping functions in clojure-mode locally.
  (defun clojure-mode-setup-paredit-keybindings ()
    (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
    (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly))
  (add-hook 'clojure-mode-hook #'clojure-mode-setup-paredit-keybindings nil 'local)

  ;; REPL history
  (define-key cider-repl-mode-map (kbd "M-r") 'cider-repl-history)

  ;; Clojure dot operator with yasnippet for Object methods completion.
  (setq yas-inhibit-overlay-modification-protection t)
  ;; (advice-add 'yas--on-protection-overlay-modification :override #'ignore) ; fix $0 placeholder gone issue.
  
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

;; [ helm-cider ] -- Helm interface to CIDER.

(use-package helm-cider
  :ensure t
  :defer t
  :after cider
  :bind (:map cider-doc-map ("c" . helm-cider-cheatsheet) ("C-c" . helm-cider-cheatsheet))
  :hook (cider-mode . helm-cider-mode))

;;; [ clj-refactor ] -- A collection of commands for refactoring Clojure code.

(use-package clj-refactor               ; [C-c C-m]
  :ensure t
  :ensure cljr-ivy
  :defer t
  :after clojure-mode
  :delight clj-refactor-mode
  :custom ((cljr-suppress-middleware-warnings t)
           (cljr-warn-on-eval nil))
  :bind (:map clojure-mode-map ("M-RET" . cljr-ivy))
  :init (add-to-list 'display-buffer-alist '("^\\*cljr-*\\*" . (display-buffer-below-selected)))
  :config
  (defun my:clj-refactor-setup ()
    (clj-refactor-mode 1)
    ;; (cljr-add-keybindings-with-prefix "C-c C-m")
    (define-key clj-refactor-map (kbd "C-c RET") #'hydra-cljr-help-menu/body))
  (add-hook 'clojure-mode-hook #'my:clj-refactor-setup)
  (add-hook 'cider-repl-mode-hook #'my:clj-refactor-setup)

  ;; FIXME: temporary solution for clj-refactor.
  (remove-hook 'find-file-hook #'cljr--ensure-no-dashes-in-filename)

  ;; fix "/" does not trigger `company-mode' auto popup.
  (with-eval-after-load 'company
    (add-to-list 'company-begin-commands 'cljr-slash)))

;;; [ debux.el ] -- Integrate Clojure/ClojureScript debugger "Debux" into Emacs.

;; https://github.com/philoskim/debux
;; (use-package debux
;;   :quelpa (debux :fetcher github :repo "philoskim/debux"))

;;; [ flycheck-clojure, squiggly-clojure ] --

;; (use-package flycheck-clojure
;;   :ensure t
;;   :defer t
;;   :after flycheck
;;   :init (flycheck-clojure-setup))

;;; [ flycheck-clj-kondo ] -- Emacs integration for clj-kondo via flycheck.

;; (use-package flycheck-clj-kondo
;;   :ensure t
;;   :after clojure-mode)

;;; [ midje-mode ] -- Minor mode for running Midje tests in Emacs.

(use-package midje-mode
  :ensure t
  :defer t
  :hook (clojure-mode . midje-mode))

;;; [ Emidje ] -- Test runner, report viewer and formatting tool for Midje within Emacs.

;; (use-package emidje
;;   :ensure t
;;   :defer t
;;   :commands (emidje-mode)
;;   :init (eval-after-load 'cider #'emidje-setup))

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

;;; [ clj-decompile ] -- Small wrapper for clj-java-decompiler in GNU Emacs on top of CIDER.

(use-package clj-decompiler
  :ensure t
  :defer t
  :commands (clj-decompiler-decompile)
  :hook (cider-mode . clj-decompiler-setup))

;;; [ cider-spy ] -- Spy on CIDER to get Info.

;; (use-package cider-spy
;;   :ensure t
;;   :defer t)

;;; [ ob-clojure ] -- org-babel support for Clojure

(use-package ob-clojure
  :defer t
  :after clojure-mode
  :commands (org-babel-execute:clojure)
  :custom (org-babel-clojure-backend 'cider)
  :config
  (add-to-list 'org-babel-load-languages '(clojure . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))

  ;; whether check Org-mode buffer's (ns ) declarations.
  ;; (setq cider-auto-track-ns-form-changes t)

  ;; let `ob-clojure' babel src blocks allow evaluation.
  (add-to-list 'org-babel-default-header-args:clojure
               '(:eval . "yes"))
  (add-to-list 'org-babel-default-header-args:clojure
               '(:noweb . "yes"))
  ;; caused tangled code wrapped in `clojure.pp/print'.
  ;; (add-to-list 'org-babel-default-header-args:clojure
  ;;              '(:results . "pp"))

  ;; No timeout when executing calls on Cider via nrepl
  (setq org-babel-clojure-sync-nrepl-timeout 30)
  (add-to-list 'org-babel-default-header-args:clojure ; for Clojure `dotimes' etc.
               '(:show-process . "no"))

  (defun ob-clojure-specify-session ()
    "Specify ob-clojure header argument :session.
With value selected from a list of available sessions."
    (interactive)
    (let ((info (org-babel-get-src-block-info)))
      (when (and (string= (car info) "clojure")
                 ;; only when :session is not specified yet.
                 (string= (cdr (assq :session (nth 2 info))) "none"))
        (org-babel-insert-header-arg
         "session"
         (format "\"%s\""
                 (completing-read
                  "Choose :session for ob-clojure: "
                  (mapcar (lambda (pair)
                            (buffer-name (car (cdr pair))))
                          (let ((sesman-system 'CIDER))
                            (sesman--all-system-sessions sesman-system 'sort)))))))))

  (define-key org-babel-map (kbd "M-j") 'ob-clojure-specify-session))

;;; [ typed-clojure-mode ] -- Typed Clojure minor mode for Emacs.

;; (use-package typed-clojure-mode
;;   :ensure t
;;   :defer t
;;   :init (add-hook 'clojure-mode-hook 'typed-clojure-mode))

;;; [ Java Docs ]

;;; `helm-dash' to search Java docs.

;;; [ helm-clojuredocs ] -- Searching for help in clojurdocs.org with Helm.

(use-package helm-clojuredocs
  :ensure t
  :defer t
  :after cider
  :commands (helm-clojuredocs)
  :bind (:map cider-doc-map ("M-d" . helm-clojuredocs)))

;;; [ clojure-essential-ref ] -- `cider-doc' to "Clojure, The Essential Reference".

(use-package clojure-essential-ref-nov
  :ensure t
  :defer t
  :commands (clojure-essential-ref clojure-essential-ref-web clojure-essential-ref-nov)
  :custom ((clojure-essential-ref-nov-epub-path
            (expand-file-name "~/Org/Wiki/Computer Technology/Programming/Programming Languages/Clojure/Data/Books/Clojure, The Essential Reference.epub")))
  :bind (:map cider-mode-map ("C-c h c" . clojure-essential-ref-nov)
              :map cider-repl-mode-map ("C-c h c" . clojure-essential-ref-nov)))

;;; [ elein ] -- running Leiningen commands from Emacs.

(use-package elein
  :ensure t
  :defer t
  :preface (let ((user-bin-path (expand-file-name "~/bin"))) ; add "lein" bin into exec-path.
             (when (file-exists-p user-bin-path)
               (add-to-list 'exec-path user-bin-path)))
  :config
  (defun elein-lein-try ()
    (interactive)
    (with-current-buffer "*scratch*"
      (setq-local inferior-lisp-program
                  (concat "lein try "
                          (read-string "dependencies: " "org.clojure/clojure 1.9.0")))
      (command-execute 'inferior-lisp))
    (rename-buffer "*elein-lein-try*")))

;;; [ clomacs ] -- Clomacs simplifies call Clojure code from Emacs lisp.

(use-package clomacs
  :ensure t
  :defer t)

;;; [ parseclj ] -- EDN reader and Clojure Parser for Emacs Lisp

(use-package parseclj
  :ensure t
  :defer t)

;;; [ cljsbuild-mode ] -- A minor mode for the ClojureScript 'lein cljsbuild' command.

(use-package cljsbuild-mode
  :ensure t
  :defer t)

(with-eval-after-load 'org
  (add-to-list 'org-default-properties "clojars")
  (add-to-list 'org-default-properties "Maven"))



(provide 'init-prog-lang-clojure)

;;; init-prog-lang-clojure.el ends here
