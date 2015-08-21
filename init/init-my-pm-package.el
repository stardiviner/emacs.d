;;; init-my-pm-package.el --- init package.el
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ package.el ] -- Emacs Lisp Package Archive (ELPA)

;;; How Packages work in Emacs 24
;;
;; Whenever Emacs starts up, it automatically calls the function ‘
;; package-initialize’ to load installed packages. This is done after loading
;; the init file and abbrev file (if any) and before running ‘after-init-hook’
;; (see Startup Summary). Automatic package loading is disabled if the user
;; option package-enable-at-startup is nil.

;;; Usage:
;; - [M-x package-list-packages] :: list out packages.
;; - [M-x package-install RET] :: install package.
;;
;; Keep in mind that MELPA packages are built automatically from the master
;; branch, meaning bugs might creep in there from time to time. Never-the-less,
;; installing from MELPA is the recommended way of obtaining CIDER, as the
;; master branch is normally quite stable and "stable" (tagged) builds are
;; released somewhat infrequently.
;;
;; With the most recent builds of Emacs, you can pin CIDER to always use MELPA
;; Stable (or Marmalade) by adding this to your Emacs initialization:
;;
;;   (add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

(require 'cl)

(require 'package)

(setq-default package-archives
	      '(("melpa" . "http://melpa.org/packages/")
		;; ("melpa-stable" . "http://stable.melpa.org/packages/")
		;; ("marmalade" . "http://marmalade-repo.org/packages/")
		("org"       . "http://orgmode.org/elpa/")
		("gnu" . "http://elpa.gnu.org/packages/")
		))

;; (setq package-user-dir "~/.emacs.d/elpa"
;;       ;; package-directory-list
;;       )


;;; Add support to package.el for pre-filtering available packages
(defvar package-filter-function nil
  "Optional predicate function used to internally filter packages used by package.el.

The function is called with the arguments PACKAGE VERSION ARCHIVE, where
PACKAGE is a symbol, VERSION is a vector as produced by `version-to-list', and
ARCHIVE is the string name of the package archive.")

(defadvice package--add-to-archive-contents
  (around filter-packages (package archive) activate)
  "Add filtering of available packages using `package-filter-function', if non-nil."
  (when (or (null package-filter-function)
	    (funcall package-filter-function
		     (car package)
		     (funcall (if (fboundp 'package-desc-version)
				  'package--ac-desc-version
				'package-desc-vers)
			      (cdr package))
		     archive))
    ad-do-it))



;; But don't take Melpa versions of certain packages
(setq package-filter-function
      (lambda (package version archive)
        (or (not (string-equal archive "melpa"))
            (not (memq package '())))))



;;; On-demand installation of packages

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))



;; (when (null package-archive-contents)
;;   (package-refresh-contents))



;; The packages that you install with package.el are activated by default after
;; your .emacs is loaded. To be able to use them before the end of your .emacs
;; you need to activate them by using the commands:
;;
;; (setq package-enable-at-startup t) ; default

;; This determines which packages should be loaded at start-up.
;; (setq package-load-list  '(all)) ; default


(package-initialize)


(defvar my-packages
  '(names
    namespaces
    let-alist ; (from GNU ELPA)
    ;; Emacs
    ;; Apperance
    color-theme
    color-theme-solarized
    page-break-lines
    ;; mode-line
    nyan-mode nyan-prompt
    ;; Keybinding
    helm-bind-key
    which-key
    hydra
    keyfreq
    showkey
    show-marks
    ;; Debug
    edebug-x
    ;; Server & Client
    edit-server
    ;; Edit
    undo-tree
    iedit multiple-cursors
    expand-region
    vlf
    ;; Navigation
    on-screen
    ace-jump-mode
    ;; Info, Man,
    info+ woman
    ;; Dired
    dired-rainbow
    dired-single dired-efap dired-details dired-narrow dired-open dired-sort dired-k ; dired-hack-utils
    direx
    peep-dired
    image-dired
    dired-dups
    ;; Sudo
    sudo-edit
    ;; Spell
    flyspell flyspell-popup flyspell-lazy helm-flyspell
    ;; Completion System
    helm
    helm-helm-commands helm-descbinds
    ;; Popup
    popup pos-tip
    showtip
    ;; Window & Session Management
    window-number
    popwin zoom-window zoom-frm
    window-purpose
    workgroups2
    ;; Mode
    diminish
    mmm-mode
    helm-mode-manager
    ;; Unicode
    helm-unicode
    pretty-mode
    ;; Color
    rainbow-mode ansi-color
    ;; Process
    async dizzee
    ;; Server
    elnode web-server websocket ; httpd
    ;; Fold
    allout allout-widgets
    foldout

    ;; Shell
    eshell ; shell-here shell-toggle
    shelldoc

    ;; Org-mode
    org
    org-bullets
    org-beautify-theme
    org-agenda-property
    org-pdfview
    org-projectile
    org-download
    org-password-manager
                                        ; org-caldav org-gcal org-mac-iCal
    orgtbl-ascii-plot
    orgit
    ob-prolog
    ob-mongo
    ob-http ob-browser
    ob-ipython
                                        ; ob-translate
    org-screenshot
    org-pomodoro
    org-trello
                                        ; org-vcard
                                        ; org-projectile org-redmine org-readme
                                        ; org-tree-slide
    
    ;; Programming
    ;; Auto Complete
    company company-statistics company-quickhelp
    ;; Snippet
    yasnippet
    helm-c-yasnippet
    ;; Template
    yatemplate skeletor
    ;; Diff
                                        ; diffview
    ;; VCS
    magit
    magit-find-file magit-filenotify magit-popup magit-tramp
    magit-gitflow magit-annex magit-gerrit magit-gh-pulls magit-stgit magit-topgit
    helm-git helm-git-files
    git-messenger
    git-gutter git-gutter+ git-gutter-fringe git-gutter-fringe+
    ;; Indent
    aggressive-indent ; auto-indent-mode
    indent-guide
    ;; Electric
    paredit
    smartparens ; autopair
    ;; Comment
    poporg ; outorg
    boxquote fic-mode
    ;; Linter
    flycheck flycheck-pos-tip helm-flycheck
    ;; Document
    dash helm-dash ; dash-at-point dash-functional
    irfc
    ;; Project Sidebar
    project-explorer sr-speedbar
    ;; Project
    projectile
    helm-projectile
    helm-cmd-t
    ;; Search
    isearch+ anzu
    swiper
    helm-c-moccur color-moccur
    ;; Regexp
    visual-regexp visual-regexp-steroids
    ample-regexps
    ;; wgrep wgrep-ack wgrep-helm
    ack
    ag helm-ag
    helm-recoll
    ;; Tags
    xcscope
    ;; Compile
    smart-compile quickrun
    ;; Make
    helm-make
    ;; Code Analyze
    cedet
    sourcegraph
    ;; License
                                        ; xlicense
    ;; Code Paste
    yagist
    ;; Bug Track System
    bts bts-github

    ;; Languages
    ;; Lisp
    rainbow-delimiters rainbow-blocks
    slime slime-annot slime-company ; slime-repl
    sly sly-company
    geiser
    eval-sexp-fu
    ;; Emacs Lisp
    elisp-slime-nav
    eldoc-eval
    hl-sexp
    highlight-symbol ; highlight-escape-sequences
    ;; C & C++
    irony irony-eldoc flycheck-irony
    company-irony company-irony-c-headers
    company-c-headers
    function-args
                                        ; company-cmake
    ;; Lua
    lua-mode
    ;; Ruby
    ruby-mode enh-ruby-mode
    inf-ruby ruby-compilation ; pry
    rbenv rvm  ; ruby-dev  ; chruby
    robe yari yard-mode
    ruby-tools
    rspec-mode minitest ruby-test-mode
    ruby-block ruby-hash-syntax ruby-interpolation
    ruby-electric ruby-end
    motion-mode
    ;; Python
    pyenv-mode pyvenv
    ;; jedi company-jedi
    anaconda-mode company-anaconda
    ;; Clojure
    clojure-mode-extra-font-locking
    inf-clojure ; clojure-env
    cider-eval-sexp-fu
    flycheck-clojure
    ;; Go
    go-mode company-go
    ;; PHP
    inf-php
    ;; Julia
    julia-mode
    ;; ESS
    ess
    ess-smart-equals ess-smart-underscore
    ess-R-data-view ess-R-object-popup
                                        ; company-ess
    ein ein-mumamo
    ;; Plot
    gnuplot
    ;; Database
    ;; SQL
    sqlup-mode
    sql-indent
    emacsql
    edbi edbi-sqlite edbi-database-url company-edbi edbi-minor-mode
    ;; db db-sql
    db-pg
    ;; sqlplus
    ;; SQLite
    sqlite
    ;; NoSQL
    mongo inf-mongo ; mongo-elnode
    ;; HTML
    emmet-mode
    ;; CSS
    css-eldoc
    show-css
    ;; Scss & Less
    scss-mode
    less-css-mode
    helm-css-scss
    ;; Slim
    slim-mode
    ;; JavaScript
    js2-mode js3-mode js-doc
    js-comint
    tern company-tern
    jst jss
    ;; Web
    web-mode
    skewer-mode
    json-mode ; json-rpc
    xml xmlunicode ; xmlgen xml-rpc

                                        ; web-beautify
    restclient httprepl
    web-completion-data
    know-your-http-well httpcode
    company-web company-restclient
    dom
    ;; Assembly
    nasm-mode iasm-mode
    ;; TeX
    company-auctex company-math
    latex-pretty-symbols
    latex-math-preview latex-preview-pane
    ;; Markdown
    markdown-mode
    ;; Sass
    sass-mode
    

    ;; Frameworks
    ;; Ruby on Rails
    projectile-rails rails-new
    rhtml-mode
    ;; Arduino
    arduino-mode company-arduino

    ;; Linux
    nginx-mode
    ;; Container
    docker dockerfile-mode
    docker-tramp
    
    ;; Tools
    ;; Bookmarks
    helm-bm
    ;; PDF
    pdf-tools
    ;; Ascii Graphic
    artist chart
    ;; Notify
    sauron
    ;; Browser
    w3m
    ;; Email
    gnus
    ;; mu4e
    mu4e-maildirs-extension
    ;; Contacts
    bbdb bbdb-vcard
    ;; Feeds: RSS & Atom
    elfeed elfeed-web
    ;; Blog
    octopress org-octopress
    ;; Calendar
    icalendar calfw calfw-gcal
    ;; Chat
    ;; IRC
                                        ; erc circe
                                        ; weechat
    ;; Music
    emms ; emms-info-media helm-emms
    ;; Screenshot & Screencast
    screenshot camcorder
    ;; Download
    ;; aria2 wget
    ;; Read
    spray
    ;; Typing
                                        ; speed-type
    ;; World Time
    world-time-mode

    ;; Languages
    ;; Chinese
    pangu-spacing
    pinyin-search
    ;; Japanese
    )
  "A list of packages to ensure are installed at launch.")

(defun my-packages-installed-p ()
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs packages is now refreshing its package database ...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p)))
  )




(provide 'init-my-pm-package)

;;; init-my-pm-package.el ends here
