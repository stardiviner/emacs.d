;;; init.el --- --- user init file  -*- no-byte-compile: t -*-

;;; Commentary:


;;; Code:

;;; load ahead

;;; increase GC at Emacs startup to speedup.
(setq emacs-start-time (float-time))
(setq gc-cons-threshold 8000000)
(add-hook
 'after-init-hook
 (lambda ()
   (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value)))))


;;; [ splash ]
(setq fancy-splash-image ; for `fancy-splash-head'
      (expand-file-name "resources/logos/my-emacs-logo.png" user-emacs-directory))
;; (setq fancy-startup-text)

;;; initial message
(setq inhibit-startup-echo-area-message "Hacking happy! stardiviner.")
(setq-default initial-scratch-message
              (concat ";; Happy Hacking " (or user-login-name "") "!\n\n"))


(require 'server)
;; (setq server-use-tcp t) ; allow for running multiple Emacs daemons.
(unless (server-running-p)
  (server-start))


;;; add my init files directory

(add-to-list 'load-path "/usr/share/emacs/site-lisp/") ; for compiled version Emacs load system installed Emacs related packages.

;;; Since Emacs 27, `user-emacs-directory' changed to "~/.config/emacs/" by default.
(if (version<= emacs-version "27")
    (setq user-emacs-directory (expand-file-name "~/.config/emacs/")))

(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))
;; recursively load init files.
(let ((default-directory (expand-file-name "init" user-emacs-directory)))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ; shadow
           (append
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

(setq load-prefer-newer t)


;;; [ package manager ]

(require 'init-package)


;;; debug, profiling etc

(require 'init-emacs-debug)
(require 'init-emacs-profiler)
(require 'init-emacs-benchmark)

(use-package pinentry
  :ensure t
  :init (pinentry-start))


;;; my custom functions

(use-package dash
  :ensure t
  :config (with-eval-after-load "dash"
            (dash-enable-font-lock)))

(require 'init-library)
(require 'init-functions)


;;; Systems

(cl-case system-type
  ('gnu/linux
   (require 'init-linux))
  ('darwin
   (require 'init-macOS))
  ('windows-nt
   (require 'init-microsoft-windows)))


;;; Emacs
(require 'init-emacs-environment)
(require 'init-emacs-settings)
(require 'init-emacs-encrypt)
(require 'init-emacs-performance)
(require 'init-emacs-security)
(require 'init-emacs-face)
(require 'init-emacs-theme)
(require 'init-emacs-appearance)
(require 'init-emacs-prettify)
;; (require 'init-emacs-emoji)
(require 'init-emacs-mode-line)
(require 'init-emacs-popup)
(require 'init-emacs-completion)
(require 'init-emacs-notify)
(require 'init-emacs-help)
(require 'init-emacs-minibuffer)
(require 'init-emacs-buffer)
(require 'init-emacs-backup)
(require 'init-emacs-window)
(require 'init-emacs-frame)
(require 'init-emacs-idle)
(require 'init-emacs-edit)
(require 'init-emacs-clipboard)
(require 'init-emacs-navigation)
(require 'init-emacs-bookmark)
(require 'init-emacs-annotate)
(require 'init-emacs-keybinding)
(require 'init-emacs-outline)
(require 'init-emacs-macro)
(require 'init-emacs-input-method)
(require 'init-emacs-file)
(require 'init-emacs-color)
(require 'init-emacs-image)
(require 'init-emacs-pdf)
(require 'init-emacs-ebook)
(require 'init-dired)
(require 'init-emacs-modes)
(require 'init-emacs-abbrev)
(require 'init-emacs-search)
(require 'init-emacs-highlight)
(require 'init-emacs-regex)
(require 'init-emacs-overlay)
(require 'init-emacs-terminal)
(require 'init-emacs-shell)
(require 'init-emacs-comint)
(require 'init-emacs-subprocess)
(require 'init-emacs-rpc)
(require 'init-emacs-network)
;; (require 'init-emacs-xwidget)
(require 'init-emacs-accessibility)
(require 'init-eaf)


;;; hypertextual information management system

(require 'init-org-mode)


;;; Vim

;; (require 'init-vim)


;;; Natural Languages

(require 'init-languages)
(require 'init-language-english)
(require 'init-language-chinese)
(require 'init-language-japanese)


;;; Tools
(unless (boundp 'tools-prefix)
  (define-prefix-command 'tools-prefix))
(global-set-key (kbd "C-x t") 'tools-prefix)

(require 'init-tool-calendar)
(require 'init-tool-dictionary)
(require 'init-tool-clock)
(require 'init-tool-pomodoro)
;; (require 'init-tool-speak)
(require 'init-tool-calculator)
(require 'init-tool-keyboard)
(require 'init-SSH)
(require 'init-tool-tmux)
(require 'init-tool-hex)
(require 'init-tool-file)
(require 'init-tool-diagram)
(require 'init-tool-ascii)
(require 'init-tool-painting)
(require 'init-tool-network)
(require 'init-tool-browser)
(require 'init-tool-downloader)
(require 'init-tool-sync)
(require 'init-tool-reading)
(require 'init-tool-email)
(require 'init-tool-feeds)
(require 'init-tool-podcast)
(require 'init-tool-contacts)
(require 'init-tool-password-manager)
(require 'init-tool-accounting)
(require 'init-tool-paste)
(require 'init-tool-irc)
;; (require 'init-communication)
(require 'init-tool-music)
;; (require 'init-tool-subtitle)
(require 'init-tool-audio)
(require 'init-tool-video)
;; (require 'init-tool-screenshot)
;; (require 'init-tool-screencast)
;; (require 'init-stack-exchange)
;; (require 'init-tool-social-network)
;; (require 'init-tool-weather)
(require 'init-tool-utilities)
;; (require 'init-leetcode)


;;; Programming
(require 'init-prog-programming)
(require 'init-prog-license)
(require 'init-prog-code)
(require 'init-prog-comment)
(require 'init-prog-indent)
(require 'init-prog-folding)
(require 'init-prog-complete)
(require 'init-prog-sense)
;; (require 'init-prog-parser)
;;; fix issue which `company-rtags' backend is before `company-irony'.
(require 'init-prog-snippet)
(require 'init-prog-template)
(require 'init-prog-sidebar)
(require 'init-prog-document)
(require 'init-prog-eval)
(require 'init-prog-compile)
(require 'init-prog-build-system)
(require 'init-prog-lint)
(unless (boundp 'debug-prefix)
  (define-prefix-command 'debug-prefix))
(global-set-key (kbd "C-c d") 'debug-prefix)
(require 'init-prog-debugger)
(require 'init-prog-profiler)
(require 'init-prog-test)
;; (require 'init-prog-test-coverage)
(require 'init-prog-reformat)
(require 'init-prog-refactor)
(require 'init-prog-project)
(require 'init-prog-vcs)
;; (require 'init-prog-bug-track-system)


;;; Programming Languages
(require 'init-prog-lsp)

(require 'init-prog-lang-lisp)
(require 'init-prog-lang-emacs-lisp)
(require 'init-prog-lang-common-lisp)
(require 'init-prog-lang-scheme)
;; (require 'init-prog-lang-racket)
;; (require 'init-prog-lang-newLisp)
;; (require 'init-prog-lang-shen)
(require 'init-prog-lang-clojure)
(require 'init-prog-lang-python)
;; (require 'init-prog-lang-ruby)
;; (require 'init-prog-lang-perl)
(require 'init-prog-lang-shell)
(require 'init-prog-lang-C-common)
;; (require 'init-prog-lang-dotnet)
;; (require 'init-prog-lang-D)
;; (require 'init-prog-lang-go)
;; (require 'init-prog-lang-rust)
;; (require 'init-prog-lang-nim)
;; (require 'init-prog-lang-lua)
;; (require 'init-prog-lang-swift)
(require 'init-prog-lang-java)
;; (require 'init-prog-lang-jvm-groovy)
;; (require 'init-prog-lang-kotlin)
;; (require 'init-prog-lang-php)
(require 'init-prog-lang-html)
(require 'init-prog-lang-css)
(require 'init-prog-lang-javascript)
;; (require 'init-prog-lang-coffeescript)
;; (require 'init-prog-lang-sibilant)
;; (require 'init-prog-lang-dart)
;; (require 'init-prog-lang-xml)
(require 'init-prog-lang-json)
(require 'init-prog-lang-csv)
;; (require 'init-prog-lang-rdf)
;; (require 'init-prog-lang-sdlang)
;; (require 'init-prog-lang-haskell)
;; (require 'init-prog-lang-ML)
;; (require 'init-prog-lang-scala)
;; (require 'init-prog-lang-elixir)
;; (require 'init-prog-lang-erlang)
;; (require 'init-prog-lang-R)
;; (require 'init-prog-lang-julia)
(require 'init-prog-lang-gnuplot)
;; (require 'init-prog-lang-octave)
;; (require 'init-prog-lang-matlab)
(require 'init-prog-lang-tex)
;; (require 'init-bibliography)
(require 'init-prog-lang-markdown)
;; (require 'init-prog-lang-reStructuredText)
;; (require 'init-prog-lang-AsciiDoc)
;; (require 'init-prog-lang-yaml)
;; (require 'init-prog-lang-toml)
;; (require 'init-prog-lang-prolog)
;; (require 'init-prog-lang-ocaml)
;; (require 'init-prog-lang-verilog)
;; (require 'init-prog-lang-assembly)
;; (require 'init-prog-lang-forth)
;; (require 'init-prog-lang-HDL)
;; (require 'init-prog-lang-applescript)
;; (require 'init-prog-lang-solidity)

;;; [ Query Languages ]

(require 'init-SQL)
;; (require 'init-NewSQL)
(require 'init-NoSQL)
;; (require 'init-CQL)
(require 'init-GraphQL)


;;; Programming Tools

(unless (boundp 'prog-tools-prefix)
  (define-prefix-command 'prog-tools-prefix))
(global-set-key (kbd "C-c t") 'prog-tools-prefix)

(require 'init-DevOps)
;; (require 'init-elasticsearch)


;;; Frameworks

(require 'init-prog-framework-web)
;; (require 'init-prog-web-browser)
(require 'init-RESTful)
;; (require 'init-prog-framework-ruby-on-rails)
;; (require 'init-prog-framework-android)
(require 'init-prog-framework-arduino)
;; (require 'init-serial-programming)
;; (require 'init-prog-framework-qt)
;; (require 'init-prog-framework-ethereum)


;;; Data Science

(require 'init-data-science)


(require 'init-log-tools)


;;; Science

;; (require 'init-academic)
;; (require 'init-math)
;; (require 'init-physics)
;; (require 'init-chemistry)
;; (require 'init-biology)
;; (require 'init-musician)


;;; Engineering

;; (require 'init-electronic)
;; (require 'init-electric-music)

;;; Hack
;; (require 'init-reverse-engineering)


;;; Authoring & Writing

(require 'init-text-checker)
;; (require 'init-authoring)

;;; Chef

(require 'init-chef)


;;; Games

;; (require 'init-games)



(require 'init-emacs-workspace)

;;; detect external system has Emacs process running?
;;; If yes, like `bug-hunter' is running. Then don't load session.

(defun how-many-emacs ()
  (let*((ps-outp (shell-command-to-string "ps -C emacs -o pid="))
        (ps-lst (split-string ps-outp)))
    (length ps-lst)))

(let ((emacs-processes (how-many-emacs)))
  (when (<= emacs-processes 1)
    (require 'init-emacs-session)))


;; (defun stardiviner-splash-animation ()
;;   "Show ASCII animation."
;;   (animate-sequence '("Fuck this shit world!"
;;                       "Author: stardiviner"
;;                       "Date: 2011/10/0 (yes, day 0!)") 0)
;;   (kill-buffer "*Animation*"))
;; (add-hook 'after-init-hook #'stardiviner-splash-animation)

;;; [ playground ] -- Manage sandboxes for alternative Emacs configurations.

;; (use-package playground
;;   :ensure t
;;   :defer t
;;   :commands (playground-checkout playground-checkout-with-options))

;;; [ splash ]

(defun my/fancy-startup-screen ()
  "My custom `fancy-startup-screen'."
  (interactive)
  (let ((splash-buffer (get-buffer-create "*GNU Emacs*")))
    (with-current-buffer splash-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq default-directory command-line-default-directory)
        (make-local-variable 'startup-screen-inhibit-startup-screen)
        (if pure-space-overflow
            (insert pure-space-overflow-message))
        (fancy-splash-head)
        (dolist (text fancy-startup-text)
          (apply #'fancy-splash-insert text)
          (insert "\n"))
        (skip-chars-backward "\n")
        (delete-region (point) (point-max))
        (insert "\n"))
      (setq tab-width 22
            buffer-read-only t)
      (set-buffer-modified-p nil)
      (if (and view-read-only (not view-mode))
          (view-mode-enter nil 'kill-buffer))
      (goto-char (point-min))
      (forward-line))
    (progn
      (split-window-below)
      (switch-to-buffer splash-buffer))))

(add-hook 'after-init-hook #'my/fancy-startup-screen)

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;;; init.el ends here
