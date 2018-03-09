;;; init-prog-lang-javascript.el --- init JavaScript for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:


;;; Code:

;;; [ JavaScript ]

(require 'js)

(setq js-indent-level 2
      js-expr-indent-offset 0
      js-paren-indent-offset 0
      ;; js-square-indent-offset 0
      ;; js-curly-indent-offset 0
      ;; js-switch-indent-offset 0
      js-flat-functions nil
      )

;; (add-to-list 'js-enabled-frameworks 'reat)

;;; auto fill-in in multi-lines comment.

(dolist (hook '(js-mode-hook
                js2-mode-hook
                js3-mode-hook
                ))
  (add-hook hook
            (lambda ()
              (setq-local comment-auto-fill-only-comments t)
              (setq-local comment-multi-line t)
              (local-set-key (kbd "RET") 'c-indent-new-comment-line)
              )
            ))


;;; helper keybindings

;; [C-o] to open a new line upper between {}.
(defun my/open-new-line-upper ()
  "[C-o] to open a new line upper."
  (interactive)
  (previous-line)
  (end-of-line)
  (newline-and-indent))

(define-key js-mode-map  (kbd "C-o") 'my/open-new-line-upper)
(with-eval-after-load "js2-mode"
  (define-key js2-mode-map (kbd "C-o") 'my/open-new-line-upper))


;;; [ javascript-mode (js-mode) ]

;; - `js-load-file' :: [C-c C-l] load source code for completion.

(with-eval-after-load 'js-mode
  (add-hook 'js-mode-hook
            (lambda ()
              ;; electric-layout-mode doesn't play nice with js-mode.
              (electric-layout-mode -1)
              )))


;;; [ ob-js ]

(require 'ob-js)

(add-to-list 'org-babel-load-languages '(js . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(add-to-list 'org-babel-tangle-lang-exts '("js" . "js"))

(with-eval-after-load 'js2-mode
  (add-to-list 'org-src-lang-modes '("js" . js2)))

(add-to-list 'org-babel-default-header-args:js
             '(:results . "output"))

;;; Session support.
;; `skewer-mode'
;; (add-to-list 'org-babel-default-header-args:js
;;              '(:session . "*skewer-repl*"))


;;; [ js2-mode ]

(use-package js2-mode
  :ensure t
  :ensure-system-package (node . "sudo pacman -S --noconfirm nodejs")
  :defer t
  :mode ("\\.js\\'" . js2-mode)
  :init
  (add-hook 'js-mode-hook 'js2-minor-mode)
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  :config
  ;; [ js2-refactor ] -- A JavaScript refactoring library for Emacs.
  ;; (use-package js2-refactor
  ;;   :ensure t
  ;;   :init
  ;;   (add-hook 'js2-mode-hook #'js2-refactor-mode)
  ;;   (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
  ;;   :config
  ;;   (js2r-add-keybindings-with-prefix "M-RET")
  ;;   )

  ;; [ xref-js2 ] -- Jump to references/definitions using ag & js2-mode's AST in Emacs.
  (use-package xref-js2
    :ensure t
    :init
    (add-hook 'js2-mode-hook
              (lambda ()
                (define-key js2-mode-map (kbd "M-.") nil)
                (add-to-list (make-local-variable 'xref-backend-functions)
                             'xref-js2-xref-backend)
                ))
    )
  )


;;; [ js3-mode ]

;; (use-package js3-mode
;;   :ensure t
;;   :defer t
;;   :mode ("\\.js\\'" . js3-mode)
;;   )


;;; [ flycheck checker ]

;; (defun js-mode-setup-flycheck-checkers ()
;;   "Disable jshint checker, so eslint will be used."
;;   ;; disable json checking (just my preference to not use it)
;;   (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
;;   (add-to-list 'flycheck-disabled-checkers 'json-jsonlist)
;;   (add-to-list 'flycheck-disabled-checkers 'javascript-jscs)
;;   ;; use eslint with web-mode for jsx files
;;   (flycheck-add-mode 'javascript-eslint 'web-mode)
;;   )
;;
;; (add-hook 'js2-mode-hook 'js-mode-setup-flycheck-checkers)
;;
;; (if (featurep 'js3-mode)
;;     (add-hook 'js3-mode-hook 'js-mode-setup-flycheck-checkers))


;;; [ nvm ] -- Manage Node versions within Emacs.

;; (use-package nvm
;;   :ensure t
;;   :defer t
;;   :config
;;   ;; FIXME: VERSION should be a number.
;;   ;; (nvm-use "system")
;;   )


;; [ js-comint ] -- a lightweight comint integration.

(use-package js-comint
  :ensure t
  :defer t
  :commands (run-js)
  :init
  (add-to-list 'display-buffer-alist
               '("^\\*JavaScript REPL\\*" (display-buffer-below-selected)))
  ;; if use node.js, we need nice output
  (setenv "NODE_NO_READLINE" "1")
  :config
  ;; (setq inferior-js-program-command "node")
  ;; (setq inferior-js-program-command "/usr/bin/java org.mozilla.javascript.tools.shell.Main")

  ;; enable tern completion in JS REPL buffer.
  (add-hook 'js-comint-mode-hook #'tern-mode)

  ;; integrate with nvm.
  (if (featurep 'nvm)
      (js-do-use-nvm))

  (dolist (hook '(js2-mode-hook
                  ;; js3-mode-hook
                  ))
    (add-hook hook '(lambda ()
                      (local-set-key (kbd "C-c C-s") 'run-js)
                      (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
                      (local-set-key (kbd "C-M-x") 'js-send-last-sexp-and-go)
                      (local-set-key (kbd "C-c C-c") 'js-send-region)
                      (local-set-key (kbd "C-c C-b") 'js-send-buffer)
                      ;; (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go)
                      (local-set-key (kbd "C-c C-l") 'js-load-file)
                      )))
  )


;;; [ nodejs-repl ] -- Run Node.js REPL and communicate the process.

;; (use-package nodejs-repl
;;   :ensure t
;;   :defer t
;;   :init
;;   (setenv "NODE_NO_READLINE" "1")
;;   )


;;; [ jscs (JavaScript Code Style) ]

;; (use-package jscs
;;   :ensure t
;;   :defer t
;;   :init
;;   ;; to apply JSCS indentation rules to JavaScript modes.
;;   (add-hook 'js-mode-hook #'jscs-indent-apply)
;;   (add-hook 'js2-mode-hook #'jscs-indent-apply)
;;   (add-hook 'js3-mode-hook #'jscs-indent-apply)
;;
;;   ;; to run "jscs --fix" on the current buffer when saving.
;;   ;; (add-hook 'js-mode-hook #'jscs-fix-run-before-save)
;;   ;; (add-hook 'js2-mode-hook #'jscs-fix-run-before-save)
;;   ;; (add-hook 'js3-mode-hook #'jscs-fix-run-before-save)
;;   )


;;; [ tern ] -- code-analysis engine for JavaScript

(use-package tern
  :ensure t
  :ensure-system-package (tern . "npm install tern")
  :defer t
  :mode ("\\.tern-project\\'" . json-mode)
  :init
  (dolist (hook '(js-mode-hook
                  js2-mode-hook
                  js3-mode-hook
                  inferior-js-mode-hook
                  ))
    (add-hook hook 'tern-mode))
  :config
  (add-hook 'tern-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-d") 'tern-get-docs)
              ))

  ;; [ company-tern ] -- Tern backend for company-mode.
  (use-package company-tern
    :ensure t
    :init
    (add-hook 'tern-mode-hook
              (lambda ()
                ;; tern-mode auto push `tern-completion-at-point' to `capf'.
                ;; (my-company-add-backend-locally 'company-jquery) ; FIXME: `company-jquery' definition is void.
                (my-company-add-backend-locally 'company-tern)
                ))
    :config
    (setq company-tern-property-marker " js" ; " ○"
          company-tern-meta-as-single-line t
          )
    )

  ;; [ tj-mode ] -- major mode for highlighting JavaScript with Tern.
  ;; (use-package tj-mode
  ;;   :ensure t
  ;;   :defer t)
  )

;;; [ skewer-mode ] -- Live interactive web development in Emacs.

(use-package skewer-mode
  :ensure t
  :defer t
  :commands (run-skewer skewer-mode skewer-html-mode skewer-css-mode)
  :init
  (skewer-setup)
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-to-list 'display-buffer-alist
               '("\\*skewer-repl\\*" . (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist
               '("\\*skewer-error\\*" . (display-buffer-below-selected)))
  )

;;; [ Indium ] -- A JavaScript development environment for Emacs.

(use-package indium
  :ensure t
  :defer t
  :commands (indium-run-node indium-run-chrome)
  :init
  (add-to-list 'display-buffer-alist
               '("\\*JS REPL\\*" . (display-buffer-below-selected)))
  :config
  (setq indium-chrome-executable "google-chrome-unstable")
  ;; live JavaScript source edit and update.
  (setq indium-update-script-on-save t)
  ;; You can setup a hook to be run after each script update.
  (add-hook 'indium-update-script-source-hook
            (lambda (url)
              (indium-eval
               (format "window.dispatchEvent(new CustomEvent('patch', {detail: {url: '%s'}}))" url))))
  )


;;; [ lsp-javascript-typescript ] -- Javascript and Typescript support for lsp-mode using javascript-typescript-langserver.

;; (use-package lsp-javascript-typescript
;;   :ensure t
;;   :defer t
;;   :init
;;   (add-hook 'js-mode-hook #'lsp-mode)
;;   (add-hook 'typescript-mode-hook #'lsp-mode) ;; for typescript support
;;   (add-hook 'js3-mode-hook #'lsp-mode) ;; for js3-mode support
;;   (add-hook 'rjsx-mode #'lsp-mode) ;; for rjsx-mode support
;;   )


;;; [ jsx-mode ] -- The XML inside of JavaScript.

;; (use-package jsx-mode
;;   :ensure t
;;   :mode ("\\.jsx\\'" . jsx-mode)
;;   :config
;;   (add-hook 'jsx-mode-hook
;;             (lambda ()
;;               (tern-mode 1)))
;;   )


;;; [ js-doc ] -- Insert JsDoc style comment easily.

;; (use-package js-doc
;;   :ensure t
;;   :init
;;   (add-hook 'js2-mode-hook
;;             #'(lambda ()
;;                 (define-key prog-comment-prefix (kbd "F") 'js-doc-insert-file-doc)
;;                 (define-key prog-comment-prefix (kbd "f") 'js-doc-insert-function-doc)
;;                 (define-key js2-mode-map (kbd "@") 'js-doc-insert-tag)))
;;   :config
;;   (setq js-doc-mail-address "numbchild@gmail.com"
;;         js-doc-author (format "stardiviner <%s>" js-doc-mail-address)
;;         js-doc-url "https://stardiviner.github.io/"
;;         js-doc-license "GPL3")
;;   )


;;; [ import-js ] -- A tool to simplify importing JS modules.

;; (use-package import-js
;;   :ensure t)

;;; [ js-import ] -- Automatically import JavaScript files from the current project or dependencies.

;; (use-package js-import
;;   :ensure t
;;   :defer t)

;;; [ js-format ] -- Format or transform code style using NodeJS server with different javascript formatter.

;; (use-package js-format
;;   :ensure t
;;   :config
;;   ;; using "standard" as js formatter
;;   (with-eval-after-load 'js2-mode
;;     (add-hook 'js2-mode-hook
;; 	      (lambda()
;; 		(js-format-setup "standard"))))
;;
;;   ;; using "jsbeautify-css" as css formatter
;;   (with-eval-after-load 'css-mode
;;     (add-hook 'css-mode-hook
;; 	      (lambda()
;; 		(js-format-setup "jsb-css"))))
;;   )

;;; [ flycheck-jest ] -- flycheck for Jest (Delightful JavaScript Testing)

;; (use-package flycheck-jest
;;   :ensure t
;;   :config
;;   (flycheck-jest-setup))


(provide 'init-prog-lang-javascript)

;;; init-prog-lang-javascript.el ends here
