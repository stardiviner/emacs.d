;;; init.el --- Emacs init file.

;;; Commentary:


;;; Code:

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



;;; [ package manager ]

;;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(setq el-get-verbose t)

;;; extra sources which not in el-get recipes.
(setq el-get-extra-sources
      '(
        ;; (:name guide-key
        ;;        :description "Guide following keys to an input key sequence automatically and dynamically in Emacs."
        ;;        :depends popwin
        ;;        :type github
        ;;        :pkgname "kbkbkbkb1/guide-key"
        ;;        :features guide-key)
	))

;;; my packages which will be installed.
(setq my-el-get-packages
      (append
       '(popup
         pos-tip
         popup-pos-tip
         showtip
         tooltip-help
         ;; Emacs
         ;; completion
         ido-vertical-mode ido-ubiquitous
         smex
         helm
         helm-descbinds helm-project
         auto-complete
         auto-complete-yasnippet auto-complete-chunk
         auto-complete-etags
         auto-complete-emacs-lisp
         auto-complete-pcmp
         company-mode
         ;; color theme
         color-theme-solarized
         ;; apperance
         highlight-symbol
         ;; modeline
         diminish pretty-mode page-break-lines
         ;; others
         ;; guru-mode
         ;; buffer & window, frame
         window-number ; switch-window
         workgroups2 ; e2wm
         popwin
         ;; minibuffer
         ;; eldoc-eval
         ;; edit
         undo-tree
         multiple-cursors
         ;; kill-ring-search
         kill-ring-ido
         ;; spell
         flyspell flyspell-guess
         ;; auto-dictionnary
         ;; imenu
         ;; jump
         ace-jump-mode
         ;; bookmark, register, macro,
         bm
         ;; keybinding
         bind-key
         guide-key ; guide-key-tip
         ;; dired
         direx
         dired-k
         ;; search
         isearch+
         anzu
         visual-regexp visual-regexp-steroids ace-jump-mode
         ack-and-a-half
         ;; Org-mode
         org-mode
         org-fstree org-bullets
         org-magit
         ;; help
         ;; discover
         ;; Email
         ;; mu mu-cite
         ;; tools
         calfw sauron ; appt
         gist
         ;; Programming
         ;; code
         ;; glasses-mode
         ;; indent
         highlight-indentation
         ;; lint
         flycheck
         ;; comment
         fic-mode
         ;; electric
         paredit autopair
         rainbow-mode rainbow-delimiters
         ;; document
         ;; snippet
         yasnippet
         ;; Tags
         cscope ; xcscope
         ascope
         ;; code browser
         sr-speedbar
         ;; compile
         smart-compile smart-compile+
         ;; vcs
         git-modes git-emacs ; git-status
         magit ; magithub
         git-gutter git-gutter-fringe ; git-gutter+
         ;; egg
         mo-git-blame
         ;; project
         projectile
         ;; languages
         ;; Ruby
         ruby-mode ruby-electric ruby-block
         yari
         auto-complete-ruby
         inf-ruby rcodetools ruby-compilation rvm robe-mode
         rspec-mode
         ;; rinari
         rhtml-mode projectile-rails
         ;; Lisp
         ;; slime
         elisp-slime-nav elisp-format eldoc-eval
         ac-slime slime-company
         ;; Scheme
         ;; Clojure
         clojure-mode ac-nrepl cider ; swank-clojure
         ;; Python
         python ; python-mode
         jedi
         ;; C family languages
         auto-complete-clang auto-complete-c-headers
         c-eldoc eassist
         ;; Go
         go-mode go-eldoc
         ;; D
         d-mode
         ;; C++
         ;; Web
         web-mode
         ;; JavaScript
         ;; js-mode
         ;; HTML
         ;; CSS
         auto-complete-css
         ;; css-mode
         css-eldoc
         ;; XML
         auto-complete-nxml
         ;; Markdown
         markdown-mode
         ;; TeX
         auto-complete-latex
         ;; ac-math
         ;;; Haskell
         haskell-mode
         ;; Verilog
         verilog-mode
         auto-complete-verilog
         ;; R
         ess
         ;; gnuplot
         gnuplot-mode
         ;; Octave
         ;; ac-octave
         ;; Frameworks
         ;; Arduino
         arduino-mode
         )
       (mapcar 'el-get-source-name el-get-extra-sources)))

(el-get 'sync my-el-get-packages)


;;; [ package.el ]

;; (require 'package)
;; (setq package-archives
;;       '(("marmalade" . "http://marmalade-repo.org/packages/")
;;         ("org"       . "http://orgmode.org/elpa/")
;;         ("melpa"     . "http://melpa.milkbox.net/packages/")))
;; (package-initialize)



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


;;; add my init files directory
(let ((default-directory "~/.emacs.d/init/"))
  (setq load-path
        (append
          (let ((load-path (copy-sequence load-path))) ; shadow
            (append
              (copy-sequence (normal-top-level-add-to-load-path '(".")))
              (normal-top-level-add-subdirs-to-load-path)))
          load-path)))



;;; debug, profiling etc
(require 'init-my-emacs-debug)

;;; my custom functions
(require 'init-my-functions)

;;; run Emacs server
(require 'server)
(unless (server-running-p)
    (server-start))

;;; Emacs
(require 'init-my-emacs-completion)
(require 'init-my-emacs-environment)
(require 'init-my-emacs-settings)
(require 'init-my-emacs-help)
(require 'init-my-emacs-apperance)
(require 'init-my-emacs-font)
(require 'init-my-emacs-popup)
(require 'init-my-emacs-highlight)
(require 'init-my-emacs-minibuffer)
(require 'init-my-emacs-buffer)
(require 'init-my-emacs-window)
(require 'init-my-emacs-edit)
(require 'init-my-emacs-bookmark)
(require 'init-my-emacs-key-bindings)
(require 'init-my-emacs-indent)
(require 'init-my-emacs-outline)
(require 'init-my-emacs-input-method)
(require 'init-my-emacs-spell)
(require 'init-my-emacs-file)
(require 'init-my-emacs-image)
(require 'init-my-emacs-dired)
(require 'init-my-emacs-tramp)
(require 'init-my-emacs-modes)
(require 'init-my-emacs-abbrev)
(require 'init-my-emacs-search)
(require 'init-my-emacs-regexp)
(require 'init-my-emacs-calendar)
(require 'init-my-emacs-vcs)
(require 'init-my-emacs-shell)
(require 'init-my-emacs-calculator)
(require 'init-my-emacs-encrypt)
(require 'init-my-emacs-customize)



;;; Tools
(require 'init-my-tool-org-mode)
(require 'init-my-tool-dict)
;; (require 'init-my-tool-bbdb)
(require 'init-my-tool-sauron)
(require 'init-my-tool-paste)
(require 'init-my-tool-w3m)
(require 'init-my-tool-diagram)
;; (require 'init-my-tool-emms)
;; (require 'init-my-tool-speak)
;; (require 'init-my-tool-newsticker)


;;; Email
(require 'init-my-email-message-mode)
(require 'init-my-email-mu4e)


;;; IRC
(require 'init-my-irc-erc)



;;; Programming
(require 'init-my-prog-code)
(require 'init-my-prog-complete)
(require 'init-my-prog-sense)
(require 'init-my-prog-cedet)
(require 'init-my-prog-indent)
(require 'init-my-prog-lint)
(require 'init-my-prog-comment)
(require 'init-my-prog-electric)
(require 'init-my-prog-tags)
;; (require 'init-my-prog-ecb)
(require 'init-my-prog-speedbar)
(require 'init-my-prog-snippet)
(require 'init-my-prog-document)
(require 'init-my-prog-compile)
(require 'init-my-prog-project)
(require 'init-my-prog-vcs-git)
(require 'init-my-prog-regexp)
(require 'init-my-prog-mmm-mode)



;;; Programming Languages
(require 'init-my-prog-lang-ruby)
(require 'init-my-prog-lang-lisp)
(require 'init-my-prog-lang-emacs-lisp)
(require 'init-my-prog-lang-common-lisp)
(require 'init-my-prog-lang-clojure)
(require 'init-my-prog-lang-C)
(require 'init-my-prog-lang-go)
(require 'init-my-prog-lang-python)
(require 'init-my-prog-lang-html)
(require 'init-my-prog-lang-html5)
(require 'init-my-prog-lang-css)
(require 'init-my-prog-lang-xml)
(require 'init-my-prog-lang-javascript)
(require 'init-my-prog-lang-haskell)
(require 'init-my-prog-lang-gnuplot)
(require 'init-my-prog-lang-octave)
(require 'init-my-prog-lang-matlab)
(require 'init-my-prog-lang-tex)
;; (require 'init-my-prog-lang-R)
(require 'init-my-prog-lang-prolog)
(require 'init-my-prog-lang-guile)


;;; Frameworks
(require 'init-my-prog-framework-web)
(require 'init-my-prog-framework-arduino)


;;; key bindings
(require 'init-my-key-bindings)



;;; at the end!!!

(require 'init-my-startup)

(set-cursor-color "cyan")

;; Nil initial/scratch buffer
(setq initial-buffer-choice nil) ; a dirty workaround for which initial buffer open replace one buffer in workgroups.
(workgroups-mode 1)

;;; init.el ends here
