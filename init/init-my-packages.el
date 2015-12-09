;;; init-my-packages.el --- init for my packages
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(defvar my-packages
  '(names
    namespaces
    let-alist ; (from GNU ELPA)
    ;; auto-compile
    use-package
    
    ;; Emacs
    ;; Apperance
    color-theme
    ;; color-theme-solarized
    page-break-lines
    ;; mode-line
    ;; nyan-mode nyan-prompt
    ;; Keybinding
    ;; helm-bind-key
    which-key
    ;; hydra
    ;; showkey
    ;; keyfreq
    ;; Debug
    edebug-x
    ;; bug-hunter
    ;; Profile & Benchmark
    ;; esup benchmark-init
    ;; Server & Client
    edit-server
    ;; View
    ;; on-screen
    ;; Macro
    ;; elmacro macrostep
    ;; Edit
    undo-tree
    iedit multiple-cursors
    expand-region
    vlf
    tablist
    ;; Narrow
    fancy-narrow ; narrow-indirect narrow-reindent recursive-narrow
    ;; narrowed-page-navigation
    ;; Outline
    ;; Navigation
    show-marks
    ;; ace-jump-mode
    ;; pophint
    ;; Fold
    origami
    ;; Info, Help, Man,
    info+
    help+ help-mode+ help-fns+
    ;; woman
    ;; Dired
    dired-rainbow
    diredful
    dired-single dired-efap dired-details dired-narrow dired-open dired-sort dired-k ; dired-hack-utils
    direx
    peep-dired
    image-dired
    dired-dups
    ;; ranger
    ;; Sudo
    ;; sudo-edit
    ;; File
    openwith
    ;; Spell
    flyspell flyspell-lazy
    helm-flyspell flyspell-popup
    ;; Completion System
    helm
    helm-helm-commands helm-descbinds
    ;; ivy counsel
    ;; Popup
    popup pos-tip
    showtip
    ;; Frame, Window & Session Management
    window-number
    ;; ace-window
    popwin ; shackle
    resize-window
    zoom-window ; zoom-frm
    ;; golden-ratio
    ;; window-purpose
    ;; e2wm elscreen
    workgroups2
    ;; Mode
    mmm-mode ; polymode
    ;; Unicode
    helm-unicode
    pretty-mode
    ;; pretty-symbols
    ;; purty-mode
    ;; Color
    ansi-color rainbow-mode kurecolor
    ;; Cursor
    ;; beacon
    ;; Idle
    ;; zone-nyan
    ;; Process
    ;; dizzee ; prodigy
    ;; Server
    ;; elnode peek-mode
    web-server websocket ; httpd
    ;; [ Other Enhancement ]
    describe-number
    
    ;; Shell
    eshell ; shell-here shell-toggle
    ;; shelldoc
    
    ;; Org-mode
    org org-plus-contrib
    org-bullets
    org-agenda-property
    org-time-budgets
    ;; org-alert
    ;; org-projectile
    org-download
    org-password-manager
    ;; org-caldav org-gcal org-mac-iCal
    orgtbl-ascii-plot
    orgit
    ob-prolog
    ob-mongo
    ob-http ob-browser
    ob-ipython
    ;; ob-translate
    org-screenshot
    ;; org-pomodoro
    ;; org-vcard
    ;; org-trello org-redmine org-readme
    ;; org-tree-slide
    
    ;; Programming
    ;; Auto Complete
    company company-quickhelp company-statistics
    ;; company-try-hard
    ;; Snippet
    yasnippet
    helm-c-yasnippet
    ;; Template
    ;; yatemplate skeletor
    ;; Diff
    diffview
    ;; VCS
    magit
    magit-find-file magit-filenotify magit-popup ; magit-tramp
    magit-gitflow
    magit-gh-pulls
    ;; magit-annex magit-gerrit magit-gh-pulls magit-stgit magit-topgit
    ;; helm-git helm-git-files
    git-messenger
    ;; git-gutter
    git-gutter+
    ;; git-gutter-fringe git-gutter-fringe+
    ;; GitHub
    ;; helm-open-github
    github-notifier
    ;; Gitlab
    ;; gitlab helm-gitlab
    ;; Indent
    aggressive-indent ; auto-indent-mode
    indent-guide
    ;; Electric
    paredit
    smartparens ; autopair
    ;; Comment
    poporg ; outorg
    boxquote
    ;; Fixme
    fic-mode
    ;; fixmee
    ;; Linter
    flycheck flycheck-pos-tip helm-flycheck
    ;; Document
    dash helm-dash dash-at-point dash-functional
    irfc
    ;; Project Sidebar
    project-explorer ; sr-speedbar
    ;; Project
    projectile
    helm-projectile
    helm-cmd-t
    ;; Search
    isearch+ anzu
    swiper
    ;; swoop helm-swoop
    helm-c-moccur color-moccur
    ;; highlight
    highlight-symbol ; highlight-thing
    ;; Regexp
    visual-regexp visual-regexp-steroids
    ample-regexps pcre2el regex-tool
    ;; wgrep wgrep-ack wgrep-helm
    ;; ack full-ack helm-ack
    ag helm-ag
    ;; pt helm-pt
    ;; helm-recoll
    ;; Tags
    ;; ctags ctags-update
    ;; rtags
    ;; helm-cscope
    ;; xcscope
    ggtags
    helm-gtags
    ;; Compile
    smart-compile ; multi-compile
    ;; Run
    quickrun
    ;; Inferior
    ;; REPL
    eval-in-repl
    ;; Scratch
    ;; scratches
    ;; Make
    ;; helm-make
    ;; Sense
    ;; cedet
    ;; sourcegraph
    ;; License
    ;; xlicense
    ;; Code Paste
    yagist ; helm-gist
    ;; Bug Track System
    ;; bts bts-github
    ;; Test
    ;; cerbere test-case-mode
    ;; runtests
    ;; Refactor
    emr
    
    ;; Languages
    ;; Lisp
    rainbow-delimiters ; rainbow-blocks
    slime slime-annot slime-company ; slime-repl
    sly sly-company
    eval-sexp-fu
    ;; Emacs Lisp
    elisp-slime-nav
    eldoc-eval
    hl-sexp ; highlight-escape-sequences
    ;; ert
    ;; ert-async  ert-runner ert-modeline ert-expectations ; ert-junit
    ;; Common Lisp
    ;; Scheme
    ;; geiser
    ;; Shell
    insert-shebang
    ;; C & C++
    irony irony-eldoc flycheck-irony
    company-irony company-irony-c-headers
    company-c-headers
    function-args
    ;; company-cmake
    cmake-ide
    rtags
    ;; Lua
    lua-mode
    ;; Ruby
    ruby-mode enh-ruby-mode ruby-additional
    inf-ruby ruby-compilation ; pry
    rbenv ; rvm  ; ruby-dev  ; chruby
    bundler
    robe ; zossima
    yari yard-mode ; helm-rb
    ruby-tools
    rspec-mode minitest ruby-test-mode
    ruby-block ruby-hash-syntax ruby-interpolation
    ;; ruby-electric ; ruby-end
    ruby-factory
    ;; ruby-guard
    ;; motion-mode
    ;; Python
    python-mode
    elpy
    pyenv-mode ; pyvenv
    ;; virtualenv virtualenvwrapper
    ;; jedi company-jedi
    anaconda-mode company-anaconda
    ;; Perl
    ;; cperl-mode perl6-mode
    ;; perl-completion plsense
    ;; helm-perldoc
    ;; flycheck-perl6
    ;; plsense-direx
    ;; Clojure
    clojure-mode
    clojure-mode-extra-font-locking
    inf-clojure ; clojure-env
    cider cider-eval-sexp-fu cider-decompile
    clojure-quick-repls
    flycheck-clojure
    ;; align-cljlet
    ;; elein
    ;; Sibilant
    sibilant-mode
    ;; Elixir
    ;; elixir-mode ; alchemist
    ;; ob-elixir
    ;; Go
    go-mode go-complete company-go go-eldoc
    gorepl-mode
    go-errcheck
    ;; go-play go-playground
    ;; gore-mode gotest
    ;; golint
    ;; go-stacktracer
    go-projectile
    ;; Swift
    swift-mode
    ;; Java
    emacs-eclim
    ;; jdee
    ;; gradle-mode
    ;; PHP
    php-mode inf-php php-eldoc
    ;; php-boris php-boris-minor-mode
    ;; phpunit
    ;; Nim
    ;; nim-mode flycheck-nim
    ;; Rust
    ;; rust-mode flycheck-rust
    ;; racer company-racer
    ;; Haskell
    haskell-mode flycheck-haskell
    ghc ghci-completion
    company-ghc company-ghci company-cabal
    ebal
    ;; Erlang
    ;; erlang
    ;; Prolog
    ;; ediprolog
    ;; IPython
    ;; ein ; ein-mumamo
    ;; ESS
    ess
    ;; Julia
    julia-mode
    julia-shell
    ;; R
    ;; ess-R-data-view ess-R-object-popup
    ;; helm-R
    ;; Processing
    ;; processing-mode
    ;; Plot
    gnuplot
    ;; Database
    ;; SQL
    ;; emacsql
    sqlup-mode format-sql sql-indent
    edbi edbi-sqlite edbi-database-url company-edbi edbi-minor-mode
    ;; db db-sql db-pg
    ;; sqlplus
    ;; SQLite
    ;; sqlite
    ;; PostgreSQL
    pg pgdevenv
    ;; NoSQL
    ;; MongoDB
    mongo inf-mongo ; mongo-elnode
    ;; Redis
    redis eredis
    ;; HTML
    emmet-mode
    impatient-mode
    ;; Haml
    haml-mode
    ;; CSS
    css-eldoc
    show-css
    flycheck-css-colorguard
    ;; Scss & Less
    sass-mode scss-mode
    less-css-mode
    ;; helm-css-scss
    ;; Slim
    slim-mode
    ;; JavaScript
    js2-mode js3-mode js-doc
    js-comint nvm
    tern company-tern tj-mode
    jscs
    jst jss
    ;; Node.js
    ;; nodejs-repl
    ;; CoffeeScript
    coffee-mode
    ;; Dart
    dart-mode
    ;; Web
    web-mode
    ;; web-beautify
    ;; multi-web-mode
    skewer-mode ; skewer-reload-stylesheets skewer-less
    livid-mode
    json-mode ; json-rpc
    ;; xml xmlunicode ; xmlgen xml-rpc
    restclient httprepl web
    web-completion-data
    know-your-http-well httpcode
    company-web company-restclient
    ;; Browser
    ;; dom
    ;; WebKit
    ;; kite jss
    ;; AngularJS
    angularjs-mode
    ;; Assembly
    ;; nasm-mode iasm-mode fasm-mode
    ;; TeX
    company-auctex company-math
    latex-pretty-symbols
    px latex-preview-pane ; latex-math-preview
    cdlatex
    ;; Markdown
    markdown-mode
    ;; YAML
    yaml-mode
    ;; pandoc-mode
    
    ;; Frameworks
    ;; Ruby on Rails
    projectile-rails rails-new ; helm-rails
    rhtml-mode
    ;; Android
    android-mode
    ;; Arduino
    arduino-mode company-arduino

    ;; Linux
    ;; emamux
    systemd
    nginx-mode
    pkgbuild-mode
    ;; stumpwm-mode
    ;; Container
    docker dockerfile-mode
    docker-tramp
    vagrant vagrant-tramp
    ;; Heroku
    heroku
    
    ;; Tools
    ;; Bookmarks
    helm-bm
    ;; helm-w3m
    helm-firefox ; helm-chrome
    ;; PDF
    pdf-tools org-pdfview
    ;; Image
    ;; image+ ; xpm
    ;; Ascii Graphic
    artist chart
    ;; Notify
    ;; sauron
    ;; Speak
    ;; Browser
    ;; w3m
    ;; Email
    ;; gnus
    ;; mu4e
    mu4e-maildirs-extension helm-mu
    ;; Contacts
    ;; bbdb
    ;; bbdb-vcard bbdb-csv-import gmail2bbdb
    ;; Feeds: RSS & Atom
    ;; elfeed elfeed-web
    ;; Blog
    ;; ox-jekyll-subtree
    octopress org-octopress
    ;; Calendar
    calfw ; calfw-gcal
    ;; Chat
    ;; IRC
    ;; circe ; erc
    ;; weechat
    ;; Music
    ;; emms helm-emms emms-player-mpv ; emms-info-media emms-mode-line-cycle
    ;; mingus
    ;; Screenshot & Screencast
    screenshot camcorder
    command-log-mode
    ;; Download
    ;; aria2 wget
    ;; Read
    spray
    ;; Typing
    ;; speed-type
    ;; World Time
    world-time-mode
    ;; Collaborate
    ;; rudel togetherly
    ;; Slides
    org-tree-slide
    ;; org-present
    ;; doc-present
    ;; Websites Client
    ;; sx
    
    ;; Emacs Lisp Libraries
    ;; Object
    ;; @
    ;; Overlay
    ;; ov
    ;; Popup
    ;; Process
    ;; multi
    ;; Widget
    ;; widget-mvc
    ;; async deferred concurrent
    ;; elisp-sandbox subemacs
    ;; fsm
    
    ;; Languages
    ;; Chinese
    ;; pangu-spacing
    pinyin-search
    ;; Japanese
    migemo
    helm-migemo avy-migemo ; ido-migemo
    )
  "A list of packages to ensure are installed at launch.")



(provide 'init-my-packages)

;;; init-my-packages.el ends here
