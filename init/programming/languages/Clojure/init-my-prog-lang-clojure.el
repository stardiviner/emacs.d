;;; init-my-prog-lang-clojure.el ---
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ clojure-mode ]

(use-package clojure-mode
  :config
  (require 'clojure-mode-extra-font-locking)
  ;; `subword-mode' is quite useful since we often have to deal with Java class
  ;; and method names.
  (add-hook 'clojure-mode-hook #'subword-mode)
  ;; (add-hook 'clojure-mode-hook 'smartparens-strict-mode)
  )


;;; [ inf-clojure ] -- basic interaction with a Clojure subprocess

;;; Usage:
;;
;; - [M-x inf-clojure] / [C-c C-z]
;; - [M-j] :: new line in sexp.

(use-package inf-clojure
  :config
  (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)
  (add-hook 'inf-clojure-mode-hook #'subword-mode)
  
  (define-key my-prog-inferior-map (kbd "c") 'inf-clojure)

  (add-hook 'inf-clojure-mode-hook
            (lambda ()
              ;; open inf-clojure inferior buffer for capf function completion.
              (unless (get-buffer-process "*inf-clojure*")
                (inf-clojure)
                (bury-buffer))
              ))
  
  ;; TODO: this might lead to `lisp-dialects-mode' hook error.
  ;; (add-hook 'inf-clojure-mode-hook
  ;;           '(lambda ()
  ;;              (cider-mode 1)
  ;;              ))
  )


;;; [ cider ] -- CIDER is a Clojure IDE and REPL for Emacs

;;; CIDER (formerly nrepl.el) is the Clojure IDE and REPL for Emacs, built on
;;; top of nREPL, the Clojure networked REPL server. It's a great alternative to
;;; the now deprecated combination of SLIME + swank-clojure.

;;; Usage:
;;
;; Use M-x run-lisp to open a simple REPL subprocess using Leiningen. Once that
;; has opened, you can use C-c C-r to evaluate the region or C-c C-l to load the
;; whole file.
;;
;; If you don't use Leiningen, you can set inferior-lisp-program to a different
;; REPL command.

;;; Connect to a running nREPL server
;; You can go to your project's dir in a terminal and type there (assuming you're using Leiningen that is):
;; - $ lein repl
;; After you get your nREPL server running go back to Emacs.
;; Typing there [M-x cider-connect] will allow you to connect to the running
;; nREPL server.

;; - [M-x cider-jack-in] / [C-c M-j] :: Launch an nREPL server and a REPL client. Prompts
;;                                      for a project root if given a prefix argument.
;; - [M-x cider-connect] / [C-c M-c] :: Connect to an already-running nREPL server.
;;
;; - [M-x cider-scratch] ::
;;
;; - `cider-doc' ::
;;
;; - `cider-load-file'  :: [C-c C-l], load source code file for completion.
;; - `inf-clojure-load-file'

;; For Debug
;;
;; Log communication with the nREPL server (extremely useful for debugging CIDER problems):
;;
;; (setq nrepl-log-messages t)

(setq nrepl-hide-special-buffers t
      ;; You can hide the *nrepl-connection* and *nrepl-server* buffers from
      ;; appearing in some buffer switching commands like switch-to-buffer(C-x
      ;; b) like this:
      ;;
      ;; When using switch-to-buffer, pressing SPC after the command will make
      ;; the hidden buffers visible. They'll always be visible in list-buffers
      ;; (C-x C-b).
      
      ;; You can control the TAB key behavior in the REPL via the
      ;; cider-repl-tab-command variable. While the default command
      ;; cider-repl-indent-and-complete-symbol should be an adequate choice for
      ;; most users, it's very easy to switch to another command if you wish
      ;; to. For instance if you'd like TAB to only indent (maybe because you're
      ;; used to completing with M-TAB) use the following snippet:
      ;;
      ;; cider-repl-tab-command 'indent-for-tab-command

      ;; Prevent the auto-display of the REPL buffer in a separate window after connection is established:
      cider-repl-pop-to-buffer-on-connect nil
      ;; Stop the error buffer from popping up while working in buffers other than the REPL:
      cider-popup-stacktraces nil
      ;; Enable error buffer popping also in the REPL:
      cider-repl-popup-stacktraces t
      ;; To auto-select the error buffer when it's displayed:
      cider-auto-select-error-buffer t
      ;; If using the wrap-stacktrace middleware from cider-nrepl, error buffer
      ;; stacktraces may be filtered by default. Valid filter types include
      ;; java, clj, repl, tooling, and dup. Setting this to nil will show all
      ;; stacktrace frames.
      cider-stacktrace-default-filters '(tooling dup)
      ;; The REPL buffer name has the format *cider-repl project-name*. Change
      ;; the separator from space to something else by overriding
      ;; nrepl-buffer-name-separator.
      nrepl-buffer-name-separator " "
      ;; The REPL buffer name can also display the port on which the nREPL
      ;; server is running. Buffer name will look like cider-repl
      ;; project-name:port.
      nrepl-buffer-name-show-port t
      ;; Make C-c C-z switch to the CIDER REPL buffer in the current window:
      cider-repl-display-in-current-window nil
      ;; Limit the number of items of each collection the printer will print to 100:
      cider-repl-print-length nil
      ;; Prevent C-c C-k from prompting to save the file corresponding to the
      ;; buffer being loaded, if it's modified:
      cider-prompt-save-file-on-load t
      ;; Change the result prefix for REPL evaluation (by default there's no prefix):
      cider-repl-result-prefix ";; => "
      ;; Change the result prefix for interactive evaluation (by default it's =>):
      cider-interactive-eval-result-prefix ";; => "
      ;; Normally code you input in the REPL is font-locked with
      ;; cider-repl-input-face (after you press RET) and results are font-locked
      ;; with cider-repl-output-face. If you want them to be font-locked as in
      ;; clojure-mode use the following:
      cider-repl-use-clojure-font-lock t
      ;; You can configure known endpoints used by the cider command offered via
      ;; a completing read. This is useful if you have a list of common
      ;; host/ports you want to establish remote nREPL connections to. Using an
      ;; optional label is helpful for identifying each host.
      ;; cider-known-endpoints '(("host-a" "10.10.10.1" "7888") ("host-b" "7888"))

      cider-repl-use-pretty-printing t
      
      ;; REPL History
      cider-repl-wrap-history t
      cider-repl-history-size 3000
      cider-repl-history-file '(expand-file-name ".temp/cider-repl-history.hist" user-emacs-directory)

      ;; error buffer popup
      ;; cider-show-error-buffer nil
      )

(add-hook 'clojure-mode-hook 'cider-mode)

(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)


(eval-after-load 'cider
  '(progn
     (add-hook 'cider-mode-hook #'eldoc-mode)
     ))

;;; auto completion
;;
;; `cider-complete-at-point'
;; integrate with company-mode

(setq cider-annotate-completion-candidates t
      ;; cider-annotate-completion-function
      cider-completion-annotations-include-ns 'always
      ;; TODO: change those symbols.
      cider-completion-annotations-alist '(("class" "c")
                                           ("field" "fi")
                                           ("function" "λ") ; f,
                                           ("import" "i")
                                           ("keyword" "k")
                                           ("local" "l")
                                           ("macro" "m")
                                           ("method" "♩") ; me,
                                           ("namespace" "n")
                                           ("protocol" "p")
                                           ("protocol-function" "pf")
                                           ("record" "r")
                                           ("special-form" "s")
                                           ("static-field" "sf")
                                           ("static-method" "sm")
                                           ("type" "t")
                                           ("var" "v"))
      cider-completion-use-context t
      )


(define-key my-prog-inferior-map (kbd "C") 'cider-scratch)


;;; [ cider-decompile ]

;;; Usage:
;;
;; - [M-x cider-decompile-func [RET] main [RET]]
;; - [M-x cider-decompile-ns-func [RET] myotherns.core/other-main [RET]]

(use-package cider-decompile
  :config
  ;; (define-key clojure-mode-map (kbd "??") cider-decompile-func)
  ;; (define-key clojure-mode-map (kbd "??") cider-decompile-ns-func)
  )


;;; [ cider-spy ] -- Spy on CIDER to get Info.

; (require 'cider-spy)


;;; [ cider-profile ]

; (require 'cider-profile)



;;; [ flycheck-clojure, squiggly-clojure ] --

(eval-after-load 'flycheck
  '(flycheck-clojure-setup))


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


;; (require 'clj-refactor)
;;
;; (add-hook 'clojure-mode-hook (lambda ()
;;                                (clj-refactor-mode 1)
;;                                ;; insert keybinding setup here
;;                                (cljr-add-keybindings-with-prefix "C-c RET")
;;                                ))
;;
;; no auto sort
;; (setq cljr-auto-sort-ns nil)
;;
;; do not prefer prefixes when using clean-ns
;; (setq cljr-favor-prefix-notation nil)


;;; [ cider-eval-sexp-fu ]

; (require 'cider-eval-sexp-fu)


;;; [ align-cljlet ]

(use-package align-cljlet
  :config
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

;; (add-hook 'clojure-mode-hook 'typed-clojure-mode)



;;; [ clojure-cheatsheet ]

;; https://github.com/clojure-emacs/clojure-cheatsheet


(provide 'init-my-prog-lang-clojure)

;;; init-my-prog-lang-clojure.el ends here
