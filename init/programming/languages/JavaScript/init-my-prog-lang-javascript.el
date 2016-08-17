;;; init-my-prog-lang-javascript.el --- init JavaScript for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:

;; http://www.emacswiki.org/emacs/JavaScript

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

;; (add-to-list js-enabled-frameworks 'reat)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))



;; auto fill-in in multi-lines comment.

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
(use-package js2-mode
  :ensure t
  :config
  (define-key js2-mode-map (kbd "C-o") 'my/open-new-line-upper))


;;; [ javascript-mode (js-mode) ]

;; - `js-load-file' :: [C-c C-l] load source code for completion.

(with-eval-after-load 'js-mode
  (add-hook 'js-mode-hook
            (lambda ()
              ;; electric-layout-mode doesn't play nice with js-mode.
              (electric-layout-mode -1)
              )))


;;; [ js2-mode ]

(use-package js2-mode
  :ensure t
  :config
  ;; (setq js2-pretty-multiline-declarations t ; 'dynamic
  ;;       js2-highlight-level 3
  ;;       )

  ;; (js2-highlight-unused-variables-mode)

  (add-hook 'js-mode-hook 'js2-minor-mode)
  )


;;; [ js3-mode ]

(use-package js3-mode
  :ensure t)


;;; [ flycheck checker ]

(defun js-mode-setup-flycheck-checkers ()
  ;; disable jshint checker, so eslint will be used.
  ;; disable json checking (just my preference to not use it)
  (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
  (add-to-list 'flycheck-disabled-checkers 'json-jsonlist)
  (add-to-list 'flycheck-disabled-checkers 'javascript-jscs)

  ;; use eslint with web-mode for jsx files
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  )

(add-hook 'js2-mode-hook 'js-mode-setup-flycheck-checkers)


;;; [ nvm ] -- Manage Node versions within Emacs.

(use-package nvm
  :ensure t
  :config
  ;; (nvm-use "system")
  )


;; [ js-comint ] -- a lightweight comint integration package, that also seems to integrate with org-mode nicely.

(use-package js-comint
  :ensure t
  :init
  (dolist (hook '(js2-mode-hook
                  js3-mode-hook
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
  :config
  ;; (setq inferior-js-program-command "node")
  ;; (setq inferior-js-program-command "/usr/bin/java org.mozilla.javascript.tools.shell.Main")

  (add-hook 'inferior-js-mode-hook
            (lambda ()
              (ansi-color-for-comint-mode-on)))

  ;; if use node.js, we need nice output
  (setenv "NODE_NO_READLINE" "1")

  ;; integrate with nvm.
  (js-do-use-nvm)
  )


;;; [ jscs (JavaScript Code Style) ]

;; (use-package jscs
;;   :ensure t
;;   :config
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


;;; Node.js

;; (defun node-repl ()
;;   (interactive)
;;   (pop-to-buffer
;;    (make-comint "node-repl" "node" nil "--interactive")))


;;; [ tern ] -- code-analysis engine for JavaScript

;;; Tern is a stand-alone code-analysis engine for JavaScript. It is intended to
;;; be used with a code editor plugin to enhance the editor's support for
;;; intelligent JavaScript editing.

;;; Usage:

;; The Emacs mode uses the bin/tern server, and project configuration is done
;; with a .tern-project file.
;;
;; The following additional keys are bound:
;;
;; - [M-tab / C-M-i]
;;     trigger completion.
;;
;; - [M-.]
;;     Jump to the definition of the thing under the cursor.
;; - [M-,]
;;     Brings you back to last place you were when you pressed M-..
;; - [C-c C-r]
;;     Rename the variable under the cursor.
;; - [C-c C-c]
;;     Find the type of the thing under the cursor.
;; - [C-c C-d]
;;     Find docs of the thing under the cursor. Press again to open the associated URL (if any).

;;; .tern-project file example:
;; {
;;   "libs": [
;;     "browser",
;;     "jquery"
;;   ],
;;   "loadEagerly": [
;;     "importantfile.js"
;;   ],
;;   "plugins": {
;;     "requirejs": {
;;       "baseURL": "./",
;;       "paths": {}
;;     }
;;   }
;; }

(use-package tern
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.tern-project\\'" . json-mode))
  :config
  (add-hook 'tern-mode-hook
            (lambda ()
              (local-set-key (kbd "C-h d d") 'tern-get-docs)
              ))
  )


;;; [ company-tern ] -- Tern backend for company-mode.

(use-package company-tern
  :ensure t
  :config
  (setq company-tern-property-marker "" ; " ○"
        company-tern-meta-as-single-line t
        )
  
  (dolist (hook '(js-mode-hook
                  js2-mode-hook
                  js3-mode-hook
                  inferior-js-mode-hook
                  ))
    (add-hook hook 'tern-mode))

  (add-hook 'tern-mode-hook
            (lambda ()
              ;; tern-mode auto push `tern-completion-at-point' to `capf'.
              (my-company-add-backend-locally 'company-jquery)
              (my-company-add-backend-locally 'company-tern)
              ))
  )


;;; [ tj-mode ] -- major mode for highlighting JavaScript with Tern.

;; (use-package tj-mode
;;   :ensure t)


;;; [ jade ] -- JavaScript Awesome Development Environment (in Emacs).

(use-package jade
  :ensure t
  :config
  ;; JavaScript evaluation in JS buffers. [C-x C-e], [C-c M-i]
  (add-hook 'js2-mode-hook #'jade-interaction-mode)
  )


;;; [ JSX-mode ] -- The XML inside of JavaScript.

(use-package jsx-mode
  ;; :ensure t
  ;; :config
  ;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
  ;;
  ;; (add-hook 'jsx-mode-hook
  ;;           (lambda ()
  ;;             (tern-mode 1)))
  )


;;; [ js-doc ] -- Insert JsDoc style comment easily.

(use-package js-doc
  :ensure t
  :config
  (setq js-doc-mail-address "numbchild@gmail.com"
        js-doc-author (format "stardiviner <%s>" js-doc-mail-address)
        js-doc-url "https://stardiviner.github.io/"
        js-doc-license "GPL3")

  (add-hook 'js2-mode-hook
            #'(lambda ()
                (define-key my-prog-comment-map (kbd "F") 'js-doc-insert-file-doc)
                (define-key my-prog-comment-map (kbd "f") 'js-doc-insert-function-doc)
                (define-key js2-mode-map (kbd "@") 'js-doc-insert-tag)))
  )


;;; JavaScript subprocess integration


;;; [ import-js ] -- A tool to simplify importing JS modules.

(use-package import-js
  ;; :ensure t
  )


;;; [ jquery-doc ] -- jQuery api documentation interface for emacs.

(use-package jquery-doc
  :ensure t
  :config
  ;; default setup.
  ;; (add-hook 'js2-mode-hook 'jquery-doc-setup)

  ;; my setup.
  (defun my-jquery-doc-setup ()
    (when (boundp 'ac-sources)
      (pushnew 'ac-source-jquery ac-sources))
    (when (boundp 'company-backends)
      ;; (pushnew 'company-jquery company-backends)
      (delq 'company-tern company-backends)
      (add-to-list (make-local-variable 'company-backends)
                   '(company-tern :with company-jquery))
      ))
  
  ;; (add-hook 'js2-mode-hook 'my-jquery-doc-setup)
  )


;;; [ xref-js2 ] -- Jump to references/definitions using ag & js2-mode's AST in Emacs.

(unless (version<= emacs-version "25")
  (use-package xref-js2
    :ensure t
    :config
    (add-hook 'js2-mode-hook
              (lambda ()
                (define-key js2-mode-map (kbd "M-.") nil)
                (add-to-list (make-local-variable 'xref-backend-functions)
                             'xref-js2-xref-backend)
                ))
    )
  )



(provide 'init-my-prog-lang-javascript)

;;; init-my-prog-lang-javascript.el ends here
