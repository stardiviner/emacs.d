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


;;; [ javascript-mode (js-mode) ]

;; - `js-load-file' :: [C-c C-l] load source code for completion.

(eval-after-load 'js-mode
  '(progn
     (add-hook 'js-mode-hook
               (lambda ()
                 (electric-layout-mode -1) ; electric-layout-mode doesn't play nice with js-mode.
                 ))))


;;; [ js2-mode ]

;; (setq js2-pretty-multiline-declarations t ; 'dynamic
;;       js2-highlight-level 3
;;       )

;; (js2-highlight-unused-variables-mode)

(add-hook 'js-mode-hook 'js2-minor-mode)


;;; [ js3-mode ]

;; (eval-after-load 'auto-complete
;;   (add-to-list 'ac-modes 'js3-mode))


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


;;; JavaScript subprocess integration


;;; [ swank-js ] -- Swank backend for Node.JS and in-browser JavaScript.

;;; Usage:
;;
;; If you want to use swank from the node project just add following to your package.json file:
;;
;;   { 
;;      "devDependencies": {
;;      "swank-js": ">=0.0.1"
;;     },
;;       "scripts": {
;;       "swank": "node node_modules/swank-js"
;;     }
;;   }
;;
;; Once this is done you should be able to run up a swank for this project by running:
;;
;;     npm run swank
;;
;; Alternatively you can install swank-js globally by running:
;;
;;     npm install -g swank-js
;;
;; Once installed you could run it from you project directory:
;;
;;     swank-js
;;
;; Make SLIME connect to the backend using M-x slime-connect and specifying
;; localhost and port 4005. You will see REPL buffer with the following prompt:
;;
;; NODE>
;;
;; This means that you're currently talking to Node.JS. You may play around with
;; it by running some JavaScript expressions.
;;
;; If you get warning about SLIME version mismatch, you may make it disappear
;; until the next SLIME upgrade by typing ,js-slime-version at the REPL and
;; entering your SLIME version (e.g. 2010-11-13.)

;;; Connecting to a web browser
;; Point your web browser to
;;
;; http://localhost:8009/swank-js/test.html
;;
;; You will see the following message appear in the REPL (browser name and
;; version may differ):
;;
;; Remote attached: (browser) Firefox3.6:127.0.0.1
;;
;; This means that the browser is now connected. Several browsers can connect
;; simultaneously and you can switch between them and Node.JS REPL using
;; ,select-remote REPL shortcut. To use it, press ',' (comma) and type
;; select-remote (completion is supported). You will see "Remote:" prompt. Press
;; TAB to see completions. Select your browser in the list by typing its name or
;; clicking on the completion. The following message will appear:
;;
;; NODE>
;; Remote selected: (browser) Firefox3.6:127.0.0.1
;; FIREFOX-3.6>
;;
;; After that, you can interactively evaluate expressions in your browser. To go
;; back to Node.JS repl, switch back to node.js/direct remote.
;;
;; FIREFOX-3.6> document.body.nodeName
;; BODY
;; FIREFOX-3.6> alert("test!")
;; FIREFOX-3.6>
;;
;; When working with browser, you may use F5 to reload the page. swank-js
;; connection with browser is lost in this case, but to solve this problem you
;; may use ,sticky-select-remote instead of ,select-remote. This way swank-js
;; will remember your selection and automagically attach to the browser whenever
;; it connects. If you press F5 after using ,sticky-select-remote, you will see
;; that browser briefly disconnects but then connects again:
;;
;; Remote detached: (browser) Firefox3.6:127.0.0.1
;; FIREFOX-3.6>
;; Remote selected (auto): (direct) node.js
;; Remote attached: (browser) Firefox3.6:127.0.0.1
;; NODE>
;; Remote selected (auto): (browser) Firefox3.6:127.0.0.1
;; FIREFOX-3.6>
;;
;; The sticky remote selection is saved in the config file, ~/.swankjsrc, so you
;; don't need to do ,sticky-select-remote after restarting the browser.

;;; Connecting to a remote page
;;
;; Now, let's try to make it work with an actual site. swank-js acts as a proxy
;; between your browser and the site so it can inject necessary script tags into
;; HTML pages and avoid cross-domain HTTP request problems. Let's point it to
;; reddit. Type ,target-url and then http://www.reddit.com (www. part is
;; important, otherwise it will redirect to www.reddit.com skipping the
;; proxy). Point your browser to http://localhost:8009, you'll see reddit
;; frontpage load. If you didn't do ,select-remote or ,sticky-select-remote yet
;; do it now and select your browser from the list of remotes. Now you can
;; execute JavaScript in the context of reddit:
;;
;; FIREFOX-3.6> $(".sitetable a.title").map(function(n) { return (n + 1) + ". " + $(this).text(); }).get().join("\n")
;; 1. Wikileaks currently under a mass DDOS attack
;; 2. Munich University - Jealous
;; ...
;;
;; Let's make a function from it. Create a file test.js somewhere and make sure
;; it uses js2-mode (if it doesn't, switch it to js2-mode using M-x
;; js2-mode). Type the following there:
;;
;; function listRedditTitles () {
;;   $(".sitetable a.title").map(
;;     function (n) {
;;       SwankJS.output((n + 1) + ". " + $(this).text() + "\n");
;;     }).get().join("\n");
;; }
;;
;; Note SwankJS.output() function being used there. It allows you to send debug print to SLIME REPL.
;;
;; Move the point somewhere into the middle of the listRedditTitles() function
;; and press C-M-x. Now you may try it out in the REPL:
;;
;; FIREFOX-3.6> listRedditTitles()
;; 1. Wikileaks currently under a mass DDOS attack
;; 2. Munich University - Jealous
;; ...
;;
;; You may edit the function definition and update it using C-M-x any number of times.

;; (define-key js2-mode-map [f6] 'slime-js-reload)
;; (define-key js3-mode-map [f6] 'slime-js-reload)

;; (add-hook 'js3-mode-hook
;;           (lambda ()
;;             ; (slime-js-minor-mode 1)
;;             ))

(add-hook 'css-mode-hook
          (lambda ()
            (define-key css-mode-map (kbd "C-M-x") 'slime-js-refresh-css)
            (define-key css-mode-map (kbd "C-c C-r") 'slime-js-embed-css)))


;;; [ nvm ] -- Manage Node versions within Emacs.

;;; Usage:
;;
;; - nvm-use (version &optional callback)
;; - nvm-use-for (version &optional callback)

(use-package nvm
  :config
  ;; (nvm-use )
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
  :init
  (dolist (hook '(js2-mode-hook
                  js3-mode-hook
                  ))
    (add-hook hook '(lambda ()
                      (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
                      (local-set-key (kbd "C-M-x") 'js-send-last-sexp-and-go)
                      (local-set-key (kbd "C-c C-c") 'js-send-region)
                      (local-set-key (kbd "C-c C-b") 'js-send-buffer)
                      ;; (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go)
                      (local-set-key (kbd "C-c C-l") 'js-load-file)
                      )))
  :config
  (setq inferior-js-program-command "node --interactive")
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

;; to apply JSCS indentation rules to JavaScript modes.
(add-hook 'js-mode-hook #'jscs-indent-apply)
(add-hook 'js2-mode-hook #'jscs-indent-apply)

;; to run "jscs --fix" on the current buffer when saving.
;; (add-hook 'js-mode-hook #'jscs-fix-run-before-save)
;; (add-hook 'js2-mode-hook #'jscs-fix-run-before-save)
;; (add-hook 'js3-mode-hook #'jscs-fix-run-before-save)


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

(add-to-list 'auto-mode-alist '("\\.tern-project\\'" . json-mode))

;; (setq tern-known-port
;;       tern-server
;;       tern-explicit-port
;;       tern-project-dir
;;       )


;;; [ company-tern ] -- Tern backend for company-mode.

(use-package company-tern
  :init
  (dolist (hook '(js-mode-hook
                  js2-mode-hook
                  js3-mode-hook
                  inferior-js-mode-hook
                  ))
    (add-hook hook
              (lambda ()
                ;; enable `tern-mode'.
                (tern-mode t)

                (my-company-add-backends-to-mode '(company-tern))
                )))
  
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

;; (require 'jsx-mode)
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
;;
;; (add-hook 'jsx-mode-hook
;;           (lambda ()
;;             (tern-mode 1)))


;;; [ skewer-mode ] --

;;; Usage:
;;
;; - [M-x ] ::


;;; [ livid-mode ] --

;;; Usage:
;;
;; - [M-x ] ::



(provide 'init-my-prog-lang-javascript)

;;; init-my-prog-lang-javascript.el ends here
