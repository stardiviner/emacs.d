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

(add-to-list 'load-path "/usr/share/emacs/site-lisp/")
(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))
;; recursively load init files.
(let ((default-directory "~/.emacs.d/init/"))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ; shadow
           (append
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

(setq load-prefer-newer t)


;;; [ package manager ]

(load "~/.emacs.d/init/init-package.el")
(require 'init-package)


;;; debug, profiling etc

(require 'init-emacs-debug)
;; (require 'init-emacs-benchmark)

(use-package pinentry
  :ensure t
  :ensure-system-package pinentry
  :config
  (pinentry-start))


;;; my custom functions

(use-package dash
  :ensure t
  :config
  (with-eval-after-load "dash"
    (dash-enable-font-lock)))

(require 'init-library)
(require 'init-functions)


;;; Emacs
(require 'init-emacs-environment)
(require 'init-emacs-settings)
(require 'init-emacs-encrypt)
(require 'init-emacs-performance)
(require 'init-emacs-security)
(require 'init-emacs-font)
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
(require 'init-emacs-vcs)
(require 'init-emacs-shell)
(require 'init-emacs-comint)
(require 'init-emacs-subprocess)
(require 'init-emacs-network)
;; (require 'init-emacs-xwidget)
(require 'init-emacs-customize)
(require 'init-emacs-accessibility)


;;; hypertextual information management system

(require 'init-org-mode)


;;; Vim

;; (require 'init-vim)


;;; Natural Languages

(require 'init-languages)
(require 'init-language-english)
;; (require 'init-language-chinese)
;; (require 'init-language-japanese)


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
;; (require 'init-tool-keyboard)
;; (require 'init-tool-tmux)
;; (require 'init-tool-hex)
(require 'init-tool-file)
(require 'init-tool-diagram)
(require 'init-tool-ascii)
;; (require 'init-tool-network)
(require 'init-tool-browser)
(require 'init-tool-downloader)
(require 'init-tool-OpenSpritz)
(require 'init-tool-email)
;; (require 'init-tool-feeds)
(require 'init-tool-contacts)
;; (require 'init-tool-password-manager)
(require 'init-tool-accounting)
(require 'init-tool-paste)
;; (require 'init-tool-collaborate)
(require 'init-tool-irc)
;; (require 'init-slack)
;; (require 'init-elfeed)
;; (require 'init-tool-music)
;; (require 'init-tool-subtitle)
;; (require 'init-tool-podcast)
;; (require 'init-tool-audio)
;; (require 'init-tool-screenshot)
;; (require 'init-tool-screencast)
;; (require 'init-stack-exchange)
;; (require 'init-tool-social-network)
;; (require 'init-tool-weather)


;;; Programming
(require 'init-prog-programming)
(require 'init-prog-license)
(require 'init-prog-code)
(require 'init-prog-comment)
(require 'init-prog-electric)
(require 'init-prog-indent)
;; (require 'init-prog-folding)
(require 'init-prog-complete)
(require 'init-prog-sense)
;; (require 'init-prog-parser)
;;; fix issue which `company-rtags' backend is before `company-irony'.
(require 'init-prog-snippet)
(require 'init-prog-template)
(require 'init-prog-sidebar)
(require 'init-prog-document)
;; (require 'init-prog-eval)
(require 'init-prog-compile)
(require 'init-prog-build-system)
(require 'init-prog-lint)
(require 'init-prog-debug)
(require 'init-prog-test)
;; (require 'init-prog-test-coverage)
(require 'init-prog-refactor)
(require 'init-prog-project)
(require 'init-prog-vcs)
;; (require 'init-prog-bug-track-system)


;;; Programming Languages
(require 'init-prog-lsp)

(require 'init-prog-lang-lisp)
(require 'init-prog-lang-emacs-lisp)
(with-eval-after-load 'lisp-mode
  (require 'init-prog-lang-common-lisp))
(with-eval-after-load 'scheme-mode
  (setq scheme-program-name "guile")
  (require 'init-prog-lang-scheme))
(with-eval-after-load 'newlisp-mode
  (require 'init-prog-lang-newLisp))
(with-eval-after-load 'shen-mode
  (require 'init-prog-lang-shen))
(with-eval-after-load 'clojure-mode
  (require 'init-prog-lang-clojure))
(with-eval-after-load 'python-mode
  (require 'init-prog-lang-python))
(with-eval-after-load 'ruby-mode
  (require 'init-prog-lang-ruby))
(with-eval-after-load 'perl-mode
  (require 'init-prog-lang-perl))
(with-eval-after-load 'sh-mode
  (require 'init-prog-lang-shell))
(with-eval-after-load 'cc-mode
  (require 'init-prog-lang-C-common))
(with-eval-after-load "init-prog-lang-C-common.el"
  (require 'init-prog-tags))
(with-eval-after-load 'csharp-mode
  (require 'init-prog-lang-csharp))
(with-eval-after-load 'fsharp-mode
  (require 'init-prog-lang-fsharp))
(with-eval-after-load 'd-mode
  (require 'init-prog-lang-D))
(with-eval-after-load 'go-mode
  (require 'init-prog-lang-go))
(with-eval-after-load 'rust-mode
  (require 'init-prog-lang-rust))
(with-eval-after-load 'nim-mode
  (require 'init-prog-lang-nim))
(with-eval-after-load 'lua-mode
  (require 'init-prog-lang-lua))
(with-eval-after-load 'swift-mode
  (require 'init-prog-lang-swift))
(with-eval-after-load 'java-mode
  (require 'init-prog-lang-java))
(with-eval-after-load 'groovy-mode
  (require 'init-prog-lang-jvm-groovy))
(with-eval-after-load 'kotlin-mode
  (require 'init-prog-lang-jvm-kotlin))
(with-eval-after-load 'php-mode
  (require 'init-prog-lang-php))
(with-eval-after-load 'html-mode
  (require 'init-prog-lang-html))
(with-eval-after-load 'css-mode
  (require 'init-prog-lang-css))
(with-eval-after-load 'js-mode
  (require 'init-prog-lang-javascript))
(with-eval-after-load 'js2-mode
  (require 'init-prog-lang-javascript))
(with-eval-after-load 'coffee-mode
  (require 'init-prog-lang-coffeescript))
(with-eval-after-load 'sibilant-mode
  (require 'init-prog-lang-sibilant))
(with-eval-after-load 'dart-mode
  (require 'init-prog-lang-dart))
(with-eval-after-load 'nxml-mode
  (require 'init-prog-lang-xml))
(with-eval-after-load 'json-mode
  (require 'init-prog-lang-json))
;; (require 'init-prog-lang-rdf)
(with-eval-after-load 'sdlang-mode
  (require 'init-prog-lang-sdlang))
(with-eval-after-load 'haskell-mode
  (require 'init-prog-lang-haskell))
(with-eval-after-load 'scala-mode
  (require 'init-prog-lang-scala))
(with-eval-after-load 'elixir-mode
  (require 'init-prog-lang-elixir))
(with-eval-after-load 'erlang-mode
  (require 'init-prog-lang-erlang))
(with-eval-after-load 'R-mode
  (require 'init-prog-lang-R))
(with-eval-after-load 'julia-mode
  (require 'init-prog-lang-julia))
(with-eval-after-load 'gnuplot-mode
  (require 'init-prog-lang-gnuplot))
(with-eval-after-load 'octave-mode
  (require 'init-prog-lang-octave))
;; (require 'init-prog-lang-matlab)
(require 'init-prog-lang-tex)
;; (require 'init-bibliography)
(with-eval-after-load 'markdown-mode
  (require 'init-prog-lang-markdown))
(with-eval-after-load 'rst-mode
  (require 'init-prog-lang-reStructuredText))
(with-eval-after-load 'yaml-mode
  (require 'init-prog-lang-yaml))
(with-eval-after-load 'prolog-mode
  (require 'init-prog-lang-prolog))
(with-eval-after-load 'tuareg-mode
  (require 'init-prog-lang-ocaml))
(with-eval-after-load 'verilog-mode
  (require 'init-prog-lang-verilog))
(with-eval-after-load 'asm-mode
  (require 'init-prog-lang-assembly))
(with-eval-after-load 'forth-mode
  (require 'init-prog-lang-forth))
(with-eval-after-load 'vhdl-mode
  (require 'init-prog-lang-HDL))
(with-eval-after-load 'applescript-mode
  (require 'init-prog-lang-applescript))

;;; [ Query Languages ]
;; SQL
(require 'init-prog-lang-database-sql)
(require 'init-prog-lang-database-sqlite)
(require 'init-prog-lang-database-mysql)
(require 'init-prog-lang-database-postgresql)
;; NewSQL
(require 'init-prog-lang-database-newsql)
;; NoSQL
(require 'init-prog-lang-database-nosql)
(require 'init-prog-lang-database-mongodb)
(require 'init-prog-lang-database-redis)
;;; CQL
(require 'init-prog-lang-database-cql)
;;; GraphQL
(require 'init-prog-lang-database-graphql)


;;; Programming Tools

(unless (boundp 'prog-tools-prefix)
  (define-prefix-command 'prog-tools-prefix))
(global-set-key (kbd "C-c t") 'prog-tools-prefix)

(require 'init-DevOps)
(with-eval-after-load 'es-mode
  (require 'init-elasticsearch))


;;; Frameworks

(require 'init-prog-framework-web)
;; (require 'init-prog-web-browser)
;; (require 'init-prog-framework-ruby-on-rails)
;; (require 'init-prog-framework-android)
(require 'init-prog-framework-arduino)
(require 'init-serial-programming)
;; (require 'init-prog-framework-qt)
;; (require 'init-prog-framework-ethereum)


;;; Data Science

(require 'init-data-science)


;;; Systems

(case system-type
  ('gnu/linux
   (require 'init-linux))
  ('darwin
   (require 'init-macOS))
  ('windows-nt
   (require 'init-microsoft-windows)))

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
(require 'init-electric-music)


;;; Authoring & Writing

(require 'init-text-checker)
(require 'init-authoring)

;;; Chef

(require 'init-chef)


;;; Games

;; (require 'init-games)



(require 'init-emacs-workspace)
(require 'init-emacs-session)

(defun stardiviner-splash-animation ()
  "Show ASCII animation."
  (animate-sequence '("Fuck this shit world!"
                      "Author: stardiviner"
                      "Date: 2011/10/0 (yes, day 0!)") 0)
  (kill-buffer "*Animation*"))
(add-hook 'after-init-hook #'stardiviner-splash-animation)

;;; [ playground ] -- Manage sandboxes for alternative Emacs configurations.

(use-package playground
  :ensure t
  :defer t
  :commands (playground-checkout playground-checkout-with-options))

;;; show Emacs initialized time.
(message "Emacs initialized in %s" (emacs-init-time))


;;; init.el ends here
