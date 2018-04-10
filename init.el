;;; init.el --- --- user init file  -*- no-byte-compile: t -*-

;;; Commentary:


;;; Code:

;;; [ splash ]
(setq fancy-splash-image ; for `fancy-splash-head'
      (expand-file-name "resources/logos/my-emacs-logo.png" user-emacs-directory))
;; (setq fancy-startup-text)

;;; initial message
(setq inhibit-startup-echo-area-message "Hacking happy! stardiviner.")
(setq-default initial-scratch-message
              (concat ";; Happy Hacking " (or user-login-name "") "!\n\n"))

;;; initial values
(defvar Info-default-directory-list nil)


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
    (dash-enable-font-lock))
  )

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
;; (require 'init-tool-downloader)
;; (require 'init-tool-OpenSpritz)
(require 'init-tool-email)
;; (require 'init-tool-feeds)
(require 'init-tool-contacts)
;; (require 'init-tool-password-manager)
(require 'init-tool-accounting)
(require 'init-tool-paste)
;; (require 'init-tool-collaborate)
;; (require 'init-tool-notify)
(require 'init-tool-irc)
;; (require 'init-slack)
(require 'init-elfeed)
(require 'init-tool-music)
;; (require 'init-tool-subtitle)
;; (require 'init-tool-podcast)
;; (require 'init-tool-audio)
(require 'init-tool-screenshot)
(require 'init-tool-screencast)
(require 'init-stack-exchange)
;; (require 'init-tool-social-network)


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
;; (require 'init-prog-language-server-protocol)

(require 'init-prog-lang-lisp)
(require 'init-prog-lang-emacs-lisp)
(require 'init-prog-lang-common-lisp)
;; (require 'init-prog-lang-lisp-scheme)
;; (require 'init-prog-lang-newLisp)
;; (require 'init-prog-lang-shen)
(require 'init-prog-lang-clojure)
(require 'init-prog-lang-python)
;; (require 'init-prog-lang-ruby)
;; (require 'init-prog-lang-perl)
(require 'init-prog-lang-shell)
(require 'init-prog-lang-C-common)
(with-eval-after-load 'init-prog-lang-C-common
  (require 'init-prog-tags))
;; (require 'init-prog-lang-csharp)
;; (require 'init-prog-lang-fsharp)
;; (require 'init-prog-lang-D)
;; (require 'init-prog-lang-go)
;; (require 'init-prog-lang-rust)
;; (require 'init-prog-lang-nim)
;; (require 'init-prog-lang-lua)
;; (require 'init-prog-lang-swift)
(require 'init-prog-lang-java)
;; (require 'init-prog-lang-jvm-groovy)
;; (require 'init-prog-lang-jvm-kotlin)
;; (require 'init-prog-lang-php)
(require 'init-prog-lang-html)
(require 'init-prog-lang-css)
(require 'init-prog-lang-javascript)
;; (require 'init-prog-lang-coffeescript)
;; (require 'init-prog-lang-sibilant)
;; (require 'init-prog-lang-dart)
(require 'init-prog-lang-query)
(require 'init-prog-lang-xml)
(require 'init-prog-lang-json)
;; (require 'init-prog-lang-rdf)
;; (require 'init-prog-lang-sdlang)
;; (require 'init-prog-lang-haskell)
;; (require 'init-prog-lang-scala)
;; (require 'init-prog-lang-elixir)
;; (require 'init-prog-lang-erlang)
(require 'init-prog-lang-R)
(require 'init-prog-lang-julia)
(require 'init-prog-lang-gnuplot)
;; (require 'init-prog-lang-octave)
;; (require 'init-prog-lang-matlab)
(require 'init-prog-lang-tex)
;; (require 'init-bibliography)
(require 'init-prog-lang-markdown)
;; (require 'init-prog-lang-reStructuredText)
(require 'init-prog-lang-yaml)
;; (require 'init-prog-lang-prolog)
;; (require 'init-prog-lang-ocaml)
;; (require 'init-prog-lang-verilog)
;; (require 'init-prog-lang-assembly)
;; (require 'init-prog-lang-forth)
;; (require 'init-prog-lang-HDL)
;; (require 'init-prog-lang-applescript)


;;; Programming Tools

(unless (boundp 'prog-tools-prefix)
  (define-prefix-command 'prog-tools-prefix))
(global-set-key (kbd "C-c t") 'prog-tools-prefix)

(require 'init-DevOps)
(require 'init-elasticsearch)


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

(require 'init-academic)
(require 'init-math)
(require 'init-physics)
(require 'init-chemistry)
(require 'init-biology)
(require 'init-musician)


;;; Engineering

(require 'init-electronic)
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
