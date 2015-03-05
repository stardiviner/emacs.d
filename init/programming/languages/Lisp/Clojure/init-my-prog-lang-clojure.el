;;; init-my-prog-lang-clojure.el ---
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ clojure-mode ]

(require 'clojure-mode)

;; provides additional font-locking for built-in methods and macros.
;;
;; The font-locking is pretty imprecise, because it doesn't take namespaces into
;; account and it won't font-lock a functions at all possible positions in a
;; sexp, but if you don't mind its imperfections you can easily enable it:
(require 'clojure-mode-extra-font-locking)


(eval-after-load 'clojure-mode
  '(progn
     (defun my-clojure-mode-defaults ()
       (clojure-test-mode +1))

     (setq my-clojure-mode-hook 'my-clojure-mode-defaults)

     (add-hook 'clojure-mode-hook
               (lambda ()
                 (run-hooks 'my-clojure-mode-hook)))))

;; smartparens is an excellent (newer) alternative to paredit. Many Clojure
;; hackers have adopted it recently and you might want to give it a try as
;; well. To enable smartparens use the following code:
;;
;; (add-hook 'clojure-mode-hook 'smartparens-strict-mode)


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

;; - [M-x cider-jack-in] :: Launch an nREPL server and a REPL client. Prompts
;;                          for a project root if given a prefix argument.
;; - [M-x cider] :: Connect to an already-running nREPL server.


(require 'cider)

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

      ;; REPL History
      cider-repl-wrap-history t
      cider-repl-history-size 1000
      cider-repl-history-file '(expand-file-name "temp/cider-repl-history.hist" user-emacs-directory)
      )

(eval-after-load 'cider
  '(progn
     (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
     
     (add-hook 'cider-repl-mode-hook
               (lambda ()
                 (paredit-mode 1)
                 (rainbow-delimiters-mode 1)))

     ;; (run-lisp)
     ))


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

(add-hook 'clojure-mode-hook 'typed-clojure-mode)



;;; [ ac-cider ]

;; (require 'ac-cider)
;;
;; (dolist (hook '(clojure-mode-hook
;;                 ))
;;   (add-hook hook 'ac-cider-setup))


;;; [ ac-nrepl ] --

;; (require 'ac-nrepl)
;;
;; (add-hook 'cider-mode-hook 'ac-nrepl-setup)
;; (add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
;; (eval-after-load 'auto-complete
;;   '(add-to-list 'ac-modes 'cider-repl-mode))
;;
;; (add-hook 'clojure-mode-hook
;;           (lambda ()
;;             (eval-after-load 'auto-complete
;;               (add-to-list 'ac-sources 'ac-nrepl))))
;;
;; (eval-after-load 'cider
;;   '(define-key cider-mode-map (kbd "M-h") 'ac-nrepl-popup-doc)
;;
;;   ;; (unless (boundp 'clojure-help-doc-map)
;;   ;;   (define-prefix-command 'clojure-help-doc-map))
;;   ;; (local-set-key (kbd "C-h d") 'clojure-help-doc-map)
;;   ;; (define-key clojure-help-doc-map (kbd "d") 'ac-nrepl-popup-doc)
;;   )
;;
;; ;;; ac-nrepl
;; (set-face-attribute 'ac-nrepl-candidate-face nil
;;                     :foreground "cyan"
;;                     :bold 'normal)
;; ;; (set-face-attribute 'ac-nrepl-selection-face nil
;; ;;                     )


;;; [ company-cider ]

(add-hook 'cider-repl-mode-hook 'company-mode)
(add-hook 'cider-mode-hook 'company-mode)


;;; [ Swank Clojure ] -- Swank Clojure is a server that allows SLIME (the Superior Lisp Interaction Mode for Emacs) to connect to Clojure projects.

;; Deprecated
;; This project is no longer under active development.
;; New users are strongly encouraged to try out nrepl.el instead. If you need an advanced debugger, Ritz might be a better fit.


;;; [ clojure-cheatsheet ]

;; https://github.com/clojure-emacs/clojure-cheatsheet


(provide 'init-my-prog-lang-clojure)

;;; init-my-prog-lang-clojure.el ends here
