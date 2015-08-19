;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

;;; init.el --- --- user init file  -*- no-byte-compile: t -*-

;;; Commentary:


;;; Code:

;;; [ profiler ]

;; (profiler-start 'cpu+mem)


;;; [ version check ]

(when (version< emacs-version "24")
  (warn "Only Emacs version 24 and up are supported."))


;;; [ Constants ]

(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-windows* (eq system-type 'windows-nt))
(defconst *is-cygwin* (eq system-type 'cygwin))
(defconst *is-mac-gui* (and *is-mac* window-system))
(defconst *is-cocoa-emacs* (and *is-mac* (eq window-system 'ns)))



(require 'server)

(unless (server-running-p)
  (server-start))



;; 1.
(setq confirm-kill-emacs 'yes-or-no-p)

;; 2.
;; `save-buffers-kill-emacs'
;; `save-buffers-kill-terminal'

;; 3.
;; (add-hook 'kill-emacs-query-functions
;;           (lambda () (y-or-n-p "Do you really want to exit Emacs? "))
;;           'append)


;;; :NOTE: some settings need to be set before required or loaded.

(setq projectile-rails-keymap-prefix (kbd "C-c C-r")) ; or [C-c C-p]


;;; add my init files directory
(let ((default-directory "~/.emacs.d/init/"))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ; shadow
           (append
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))


;;; [ auto-compile ]

;;; This package provides two minor modes which automatically recompile Emacs
;;; Lisp source files. Together these modes guarantee that Emacs never loads
;;; outdated byte code files.

;;; Usage:
;;
;; [M-x auto-compile-display-log]

;; To reduce the risk of loading outdated byte code files, enable
;; auto-compile-on-load-mode as early as possible, preferably even before the
;; package manager.

;; (setq load-prefer-newer t)
;;
;; (add-to-list 'load-path (concat user-emacs-directory "el-get/packed/packed.el"))
;; (add-to-list 'load-path (concat user-emacs-directory "el-get/dash/dash.el"))
;; (add-to-list 'load-path (concat user-emacs-directory "el-get/auto-compile/auto-compile.el"))
;; (load (concat user-emacs-directory "el-get/packed/packed.el"))
;; (load (concat user-emacs-directory "el-get/dash/dash.el"))
;; (load (concat user-emacs-directory "el-get/auto-compile/auto-compile.el"))
;;
;; (require 'auto-compile)
;; (auto-compile-on-load-mode 1)
;; (auto-compile-on-save-mode 1)
;;
;; (setq auto-compile-display-buffer nil)
;; (setq auto-compile-mode-line-counter t)


;;; [ Benchmark ]
;; (let ((benchmark-init.el (expand-file-name  "el-get/benchmark-init/benchmark-init.el" user-emacs-directory)))
;;   (when (file-exists-p benchmark-init.el)
;;     (load benchmark-init.el)))

;;; Usage:
;; - [benchmark-init/show-durations-tabulated] ::
;; - [benchmark-init/show-durations-tree] ::
;; - [benchmark-init/activate]
;; - [benchmark-init/deactivate]


;;; [ package manager ]

;; (load "~/.emacs.d/init/Emacs/init-my-emacs-package-management.el")
;; (require 'init-my-emacs-package-management)

(load "~/.emacs.d/init/init-my-pm-el-get.el")
(require 'init-my-pm-el-get)

;; (load (expand-file-name "~/.emacs.d/init/init-my-pm-package.el" user-emacs-directory))
(load "~/.emacs.d/init/init-my-pm-package.el")
(require 'init-my-pm-package)




;;; Bootstrapping el-get + packages

;; (setq el-get-sources
;; '((:name package-name)))

;; (defun sync-packages ()
;; "Synchronize packages"
;; (interactive)
;; (el-get 'sync '(el-get package))
;; (add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (setq my-packages (mapcar 'el-get-source-name el-get-sources))
;; (el-get 'sync my-packages))

;; (if (require 'el-get nil t)
;; (sync-packages)
;; (url-retrieve
;; "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
;; (lambda (s)
;; (let (el-get-master-branch)
;; (end-of-buffer)
;; (eval-print-last-sexp)
;; (setq el-get-verbose t)
;; (sync-packages)))))


;; (require 'init-my-pm-paradox)


;;; namespace

;; NOTE: fix package "names" not available for package upgrade like aggressive-indent-mode.
;;
;; `define-namespace' is autoloaded, so there's no need to require
;; `names'. However, requiring it here means it will also work for people who
;; don't install through package.el.
(eval-when-compile (require 'names))
(require 'names-dev)


;;; debug, profiling etc
(require 'init-my-emacs-debug)

;;; benchmark
;; (require 'init-my-emacs-benchmark)

;;; my custom functions
(require 'init-my-library)
(require 'init-my-functions)


;;; key bindings
(require 'init-my-keybindings)


;;; Emacs
(require 'init-my-emacs-color-theme)
(require 'init-my-emacs-apperance)
(require 'init-my-emacs-font)
(require 'init-my-emacs-highlight)
(require 'init-my-emacs-completion)
(require 'init-my-emacs-popup)
(require 'init-my-emacs-notify)
(require 'init-my-emacs-environment)
(require 'init-my-emacs-settings)
(require 'init-my-emacs-help)
(require 'init-my-emacs-minibuffer)
(require 'init-my-emacs-buffer)
(require 'init-my-emacs-window)
(require 'init-my-emacs-edit)
(require 'init-my-emacs-bookmark)
(require 'init-my-emacs-keybinding)
(require 'init-my-emacs-indent)
(require 'init-my-emacs-outline)
(require 'init-my-emacs-input-method)
(require 'init-my-emacs-spell)
(require 'init-my-emacs-file)
(require 'init-my-emacs-tramp)
(require 'init-my-emacs-image)
(require 'init-my-emacs-pdf)
(require 'init-my-emacs-dired)
(require 'init-my-emacs-modes)
(require 'init-my-emacs-abbrev)
(require 'init-my-emacs-search)
(require 'init-my-emacs-regexp)
(require 'init-my-emacs-vcs)
(require 'init-my-emacs-shell)
(require 'init-my-emacs-subprocess)
(require 'init-my-emacs-encrypt)
(require 'init-my-emacs-customize)
(require 'init-my-emacs-tabulate)
;; (require 'init-my-emacs-japanese)


;;; Tools
(require 'init-my-tool-org-mode)
(require 'init-my-tool-calendar)
(require 'init-my-tool-dict)
(require 'init-my-tool-clock)
;; (require 'init-my-tool-speak)
(require 'init-my-tool-calculator)
(require 'init-my-tool-diagram)
(require 'init-my-tool-bbdb)
(require 'init-my-tool-browser)
(require 'init-my-tool-OpenSpritz)
;;; Email
(require 'init-my-tool-email-message-mode)
(require 'init-my-tool-email-mu4e)
(require 'init-my-tool-feeds)
(require 'init-my-tool-blog)
(require 'init-my-tool-paste)
;; (require 'init-my-tool-sauron)
;;; IRC
;; (require 'init-my-tool-irc-erc)
;; (require 'init-my-tool-w3m)
(require 'init-my-tool-emms)
(require 'init-my-tool-screenshot)
(require 'init-my-tool-screencast)
;; (require 'init-my-tool-stack-exchange)


;;; Programming
(require 'init-my-prog-programming)
(require 'init-my-prog-code)
(require 'init-my-prog-complete)
(require 'init-my-prog-sense)
(require 'init-my-prog-indent)
(require 'init-my-prog-lint)
(require 'init-my-prog-comment)
(require 'init-my-prog-electric)
(require 'init-my-prog-tags)
(require 'init-my-prog-sidebar)
(require 'init-my-prog-snippet)
(require 'init-my-prog-template)
(require 'init-my-prog-document)
(require 'init-my-prog-compile)
(require 'init-my-prog-project)
(require 'init-my-prog-vcs)
;; (require 'init-my-prog-bug-track-system)
;; (require 'init-my-prog-refactor)



;;; Programming Languages
(require 'init-my-prog-lang-lisp)
(require 'init-my-prog-lang-emacs-lisp)
(require 'init-my-prog-lang-common-lisp)
;; (require 'init-my-prog-lang-guile)
(require 'init-my-prog-lang-clojure)
(require 'init-my-prog-lang-ruby)
(require 'init-my-prog-lang-python)
(require 'init-my-prog-lang-C-common)
(require 'init-my-prog-lang-C)
(require 'init-my-prog-lang-go)
(require 'init-my-prog-lang-lua)
(require 'init-my-prog-lang-swift)
(require 'init-my-prog-lang-html)
(require 'init-my-prog-lang-css)
(require 'init-my-prog-lang-javascript)
(require 'init-my-prog-lang-database)
(require 'init-my-prog-lang-xml)
(require 'init-my-prog-lang-json)
;; (require 'init-my-prog-lang-haskell)
(require 'init-my-prog-lang-elixir)
;; (require 'init-my-prog-lang-erlang)
(require 'init-my-prog-lang-ESS)
(require 'init-my-prog-lang-R)
(require 'init-my-prog-lang-julia)
(require 'init-my-prog-lang-gnuplot)
;; (require 'init-my-prog-lang-octave)
;; (require 'init-my-prog-lang-matlab)
;; (require 'init-my-prog-lang-tex)
(require 'init-my-prog-lang-markdown)
;; (require 'init-my-prog-lang-prolog)
;; (require 'init-my-prog-lang-verilog)
;; (require 'init-my-prog-lang-assembly)


;;; Frameworks
(require 'init-my-prog-framework-web)
;; (require 'init-my-prog-framework-arduino)
(require 'init-my-prog-ruby-on-rails)
(require 'init-my-prog-nginx)


;;; programming related tools
(require 'init-my-prog-container)


;;; at the end!!!

(require 'init-my-startup)


;; Nil initial/scratch buffer
(setq initial-buffer-choice nil) ; a dirty workaround for which initial buffer open replace one buffer in workgroups.
(workgroups-mode 1)


;;; init.el ends here
