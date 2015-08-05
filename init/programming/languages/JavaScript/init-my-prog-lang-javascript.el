;;; init-my-prog-lang-javascript.el --- init JavaScript for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:

;; http://www.emacswiki.org/emacs/JavaScript

;;; Code:

;;; [ JavaScript ]

(add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))


;;; [ javascript-mode (js-mode) ]

(eval-after-load 'js-mode
  '(progn
     (add-hook 'js-mode-hook
               (lambda ()
                 (electric-layout-mode -1) ; electric-layout-mode doesn't play nice with js-mode.
                 ))))


;;; [ js2-mode ]

;; (autoload 'js2-mode "js2-mode" nil t)
;;
;; (eval-after-load 'auto-complete
;;   (add-to-list 'ac-modes 'js2-mode))


;;; [ js3-mode ]

(require 'js3-mode)

;; (eval-after-load 'auto-complete
;;   (add-to-list 'ac-modes 'js3-mode))


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


;; [ js-comint ] -- a lightweight comint integration package, that also seems to integrate with org-mode nicely.

;; ElnodeIJS, NicFerrier
;; having a go at writing an Emacs to JavaScript shell. It uses comet for
;; communication. Currently here and in the Elnode source tree.

;; jsSlime
;; provides an Emacs interface to the browser’s debugger and javascript engine.

;;; Usage:
;;
;; - `run-js'
;; - `send-region'

(require 'js-comint)

(setq inferior-js-program-command "node --interactive")
;; (setq inferior-js-program-command "/usr/bin/java org.mozilla.javascript.tools.shell.Main")

(add-hook 'js2-mode-hook '(lambda ()
             (local-set-key "\C-x\C-e" 'js-send-last-sexp)
             (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
             (local-set-key "\C-cb" 'js-send-buffer)
             (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
             (local-set-key "\C-cl" 'js-load-file-and-go)
             ))

;; if use node.js, we need nice output
(setenv "NODE_NO_READLINE" "1")


;;; Node.js

;; (defun node-repl ()
;;   (interactive)
;;   (pop-to-buffer
;;    (make-comint "node-repl" "node" nil "--interactive")))


;;; [ tern ] -- code-analysis engine for JavaScript

;;; http://ternjs.net/

;;; Tern is a stand-alone code-analysis engine for JavaScript. It is intended to
;;; be used with a code editor plugin to enhance the editor's support for
;;; intelligent JavaScript editing.

;; $ npm install tern

;;; Usage:

;; The Emacs mode uses the bin/tern server, and project configuration is done with a .tern-project file.
;;
;; Buffers in tern-mode add a completion-at-point function that activates Tern’s completion. So, unless you rebound the key, M-tab (or C-M-i) will trigger completion.
;;
;; When the point is in an argument list, Tern will show argument names and types at the bottom of the screen.
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

(require 'tern)
(autoload 'tern-mode "tern.el" nil t)

;; (dolist (hook '(js-mode-hook
;;                 js2-mode-hook
;;                 js3-mode-hook
;;                 ))
;;   (add-hook hook
;;             (lambda ()
;;               (tern-mode t))))

;; (setq tern-known-port
;;       tern-server
;;       tern-explicit-port
;;       tern-project-dir
;;       )


;;; for auto-complete

;; (eval-after-load 'tern
;;   '(progn
;;      (require 'tern-auto-complete)
;;      (tern-ac-setup)))


;;; [ company-tern ] -- Tern backend for company-mode.

(require 'company-tern)

(dolist (hook '(js-mode-hook
                js2-mode-hook
                js3-mode-hook
                inferior-js-mode-hook
                ))
  (add-hook hook
            (lambda ()
              ;; enable `tern-mode'.
              (tern-mode t)

              (add-to-list (make-local-variable 'company-backends)
                           'company-tern)
              
              ;;; mode locally for company-mode backend.
              ;; (make-local-variable 'company-backends)
              ;; (add-to-list 'company-backends 'company-tern)
              )))


;; (setq company-tern-property-marker "" ; remove circles after object's own properties.
;;       company-tern-meta-as-single-line t ; trim too long function signatures to the frame width.
;;       ;; If you doesn't like inline argument annotations appear with
;;       ;; corresponding identifiers, then you can to set up the company align
;;       ;; option.
;;       company-tooltip-align-annotations t
;;       )



(provide 'init-my-prog-lang-javascript)

;;; init-my-prog-lang-javascript.el ends here
