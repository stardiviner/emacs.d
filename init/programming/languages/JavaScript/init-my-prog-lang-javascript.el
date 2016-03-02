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
(use-package js3-mode
  :ensure t
  :config
  (define-key js3-mode-map (kbd "C-o") 'my/open-new-line-upper))


;;; [ javascript-mode (js-mode) ]

;; - `js-load-file' :: [C-c C-l] load source code for completion.

(with-eval-after-load 'js-mode
  (add-hook 'js-mode-hook
            (lambda ()
              (electric-layout-mode -1) ; electric-layout-mode doesn't play nice with js-mode.
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

;; (use-package js3-mode
;;   :ensure t
;;   :config
;;   ;; (add-to-list 'ac-modes 'js3-mode)
;;   )


;;; [ flycheck checker ]

;; disable jshint checker, so eslint will be used.
;; disable json checking (just my preference to not use it)
;; add javascript-jshint and json-jsonlist to the flycheck-disabled-checkers list.
(add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
(add-to-list 'flycheck-disabled-checkers 'json-jsonlist)

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)


;;; [ ac-js2 ] -- Javascript auto-completion in Emacs using Js2-mode's parser and Skewer-mode.

;;; Usage:
;;
;; Call run-skewer and open up a Javascript file. Type part of a variable and
;; call completion-at-point or if you have auto-complete installed wait for the
;; popup.

;; (add-hook 'js2-mode-hook 'ac-js2-mode)
;;
;; (setq ac-js2-evaluate-calls t ; if you want to evaluate your Javascript code for
;;                               ; candidates. Not setting this value will still
;;                                         ; provide you with basic completion.
;;       ;; ac-js2-external-libraries '("full/path/to/a-library.js")
;;       )


;;; [ nvm ] -- Manage Node versions within Emacs.

;;; Usage:
;;
;; - nvm-use (version &optional callback)
;; - nvm-use-for (version &optional callback)

(use-package nvm
  :ensure t
  :config
  ;; (nvm-use "system")
  )


;; [ js-comint ] -- a lightweight comint integration package, that also seems to integrate with org-mode nicely.

;; ElnodeIJS, NicFerrier
;; having a go at writing an Emacs to JavaScript shell. It uses comet for
;; communication. Currently here and in the Elnode source tree.

;; jsSlime
;; provides an Emacs interface to the browser’s debugger and javascript engine.

;;; Usage:
;;
;; - `inferior-js-mode'
;; - `js-send-region' & `js-send-buffer'
;; - `js-load-file' & `js-load-file-and-go'
;; - `run-js'
;; - `send-region'
;; - `switch-to-js'

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


;;; [ JSCS (JavaScript Code Style) ]

;;; Usage:
;;
;; - [M-x jscs-fix] :: "$ jscs --fix"

(use-package jscs
  :ensure t
  :config
  ;; to apply JSCS indentation rules to JavaScript modes.
  (add-hook 'js-mode-hook #'jscs-indent-apply)
  (add-hook 'js2-mode-hook #'jscs-indent-apply)
  (add-hook 'js3-mode-hook #'jscs-indent-apply)

  ;; to run "jscs --fix" on the current buffer when saving.
  ;; (add-hook 'js-mode-hook #'jscs-fix-run-before-save)
  ;; (add-hook 'js2-mode-hook #'jscs-fix-run-before-save)
  ;; (add-hook 'js3-mode-hook #'jscs-fix-run-before-save)
  )


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
  ;; (setq tern-known-port
  ;;       tern-server
  ;;       tern-explicit-port
  ;;       tern-project-dir
  ;;       )
  )


;;; [ company-tern ] -- Tern backend for company-mode.

(use-package company-tern
  :ensure t
  :init
  (dolist (hook '(js-mode-hook
                  js2-mode-hook
                  js3-mode-hook
                  inferior-js-mode-hook
                  ))
    (add-hook hook 'tern-mode))

  (add-hook 'tern-mode-hook
            '(lambda ()
               ;; tern-mode auto push `tern-completion-at-point' to `capf'.
               (my-company-add-backends-to-mode '(company-tern))
               ))
  
  :config
  (setq
   company-tern-property-marker "" ; " ○"
   company-tern-meta-as-single-line t
   )
  )


;;; [ tj-mode ] -- Highlight JavaScript with Tern.

;; FIXME:
;; (add-hook 'js3-mode-hook
;;           (lambda ()
;;             (tj-mode 1)
;;             ))


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




(provide 'init-my-prog-lang-javascript)

;;; init-my-prog-lang-javascript.el ends here
