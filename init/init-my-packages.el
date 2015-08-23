;;; init-my-packages.el --- init for my packages
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(defvar my-packages
  '(names
    namespaces
    let-alist ; (from GNU ELPA)
    paradox
    use-package
    
    ;; Emacs
    ;; Apperance
    color-theme
    color-theme-solarized
    monokai-theme
    page-break-lines
    ;; mode-line
    nyan-mode nyan-prompt
    ;; Keybinding
    helm-bind-key
    which-key
    ;; hydra
    ;; showkey
    ;; keyfreq
    ;; Debug
    edebug-x
    ;; bug-hunter
    ;; Server & Client
    edit-server
    ;; View
    ;; on-screen
    ;; Edit
    undo-tree
    iedit multiple-cursors
    expand-region
    vlf
    ;; Navigation
    show-marks
    ;; ace-jump-mode
    ;; Info, Man,
    info+ ; woman
    ;; Dired
    dired-rainbow
    dired-single dired-efap dired-details dired-narrow dired-open dired-sort dired-k ; dired-hack-utils
    direx
    peep-dired
    image-dired
    dired-dups
    ;; Sudo
    ;; sudo-edit
    ;; Spell
    flyspell flyspell-lazy
    helm-flyspell ; flyspell-popup
    ;; Completion System
    helm
    helm-helm-commands helm-descbinds
    ;; Popup
    popup pos-tip
    showtip
    ;; Frame, Window & Session Management
    window-number
    popwin zoom-window ; zoom-frm
    ;; window-purpose
    workgroups2
    ;; Mode
    mmm-mode ; polymode
    ;; Unicode
    ;; helm-unicode
    ;; pretty-mode
    ;; pretty-symbols
    ;; purty-mode
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
    ;; shelldoc

    ;; Org-mode
    org org-plus-contrib
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
    magit-gitflow
    ;; magit-annex magit-gerrit magit-gh-pulls magit-stgit magit-topgit
    ;; helm-git helm-git-files
    git-messenger
    git-gutter git-gutter+
    ;; git-gutter-fringe git-gutter-fringe+
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
    dash helm-dash dash-at-point dash-functional
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
    ample-regexps pcre2el
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
    ;; Sense
    cedet
    sourcegraph
    ;; License
                                        ; xlicense
    ;; Code Paste
    yagist helm-gist
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
    hl-sexp highlight-symbol highlight-escape-sequences
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
    robe yari yard-mode ; helm-rb
    ruby-tools
    rspec-mode minitest ruby-test-mode
    ruby-block ruby-hash-syntax ruby-interpolation
    ruby-electric ruby-end
    ;; motion-mode
    ;; Python
    pyenv-mode pyvenv
    virtualenv virtualenvwrapper
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
    sqlup-mode format-sql sql-indent
    emacsql
    edbi edbi-sqlite edbi-database-url company-edbi edbi-minor-mode
    ;; db db-sql
    db-pg
    ;; sqlplus
    ;; SQLite
    sqlite
    ;; PostgreSQL
    pg pgdevenv
    ;; NoSQL
    ;; MongoDB
    mongo inf-mongo ; mongo-elnode
    ;; Redis
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
    pandoc-mode
    ;; Sass
    sass-mode
    

    ;; Frameworks
    ;; Ruby on Rails
    projectile-rails rails-new ; helm-rails
    rhtml-mode
    ;; Arduino
    arduino-mode company-arduino

    ;; Linux
    systemd
    nginx-mode
    pkgbuild-mode
    ;; Container
    docker dockerfile-mode
    docker-tramp
    vagrant vagrant-tramp
    ;; Heroku
    heroku
    
    ;; Tools
    ;; Bookmarks
    helm-bm
    helm-firefox ; helm-chrome
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
    mu4e-maildirs-extension helm-mu
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



(provide 'init-my-packages)

;;; init-my-packages.el ends here
