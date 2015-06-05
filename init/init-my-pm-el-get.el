;;; init-my-pm-el-get.el --- init el-get
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; el-get

;;; Usage:
;;
;; - auto remove disabled packages. `(el-get-cleanup my:el-get-packages)'
;; - `el-get-list-packages' :: list out all packages.

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
(setq my:el-get-packages
      (append
       '(;; debug
         edebug-x
         ;; debuggs
         bug-hunter
         ;; benchmark
         ;; benchmark-init
         ;; package
         ;; paradox
         ;; Emacs self
         ;; macro
         ;; elmacro macrostep
         ;; popup
         popup
         pos-tip
         popup-pos-tip
         showtip
         tooltip-help
         ;; Emacs
         ;; auto-compile
         ;; completion
         ;; ido-vertical-mode ido-ubiquitous ido-better-flex smex ido-hacks
         helm
         helm-helm-commands helm-descbinds ; helm-bind-key helm-themes
         ;; helm-package
         ;; FIXME helm-c-moccur
         helm-cmd-t
         helm-c-yasnippet helm-c-flycheck ; helm-flycheck helm-flyspell
         helm-c-moccur ; helm-ack
         ;; helm-shell helm-shell-history
         helm-gtags helm-yaetags
         helm-mu
         ;; helm-mode-manager
         helm-bm helm-chrome helm-firefox
         ;; helm-w3m helm-webkit
         ;; helm-delicious
         ;; helm-dictionary
         ;; helm-google
         ;; helm-make
         ;; helm-go-package
         ;; helm-css-scss
         ;; helm-R
         ;; helm-perldoc
         ;; helm-proc
         ;; helm-ls-git
         helm-git helm-git-files ; helm-git-grep
         helm-gitlab
         ;; Auto Complete
         ;; auto-complete
         ;; auto-complete-yasnippet auto-complete-chunk
         ;; auto-complete-etags
         ;; auto-complete-emacs-lisp
         ;; auto-complete-pcmp
         ;; ac-capf
         ;; ac-helm
         ;; ac-company
         company-mode company-quickhelp company-statistics
         ;; color theme
         color-theme-solarized
         ;; color-theme-monokai ; color-theme-almost-monokai monokai-theme
         ;; apperance
         ;; emacs-powerline
         ;; highlight-cl highlight-fns highlight-defined
         ;; highlight-sexp highlight-blocks highlight-quoted ; highlight-stages
         ;; highlight-escape-sequences ; highlight-numbers highlight-parentheses
         ;; cursor-chg col-highlight
         ;; yascroll
         ;; minimap sublimity
         pretty-mode-plus page-break-lines
         ;; mode-line
         diminish nyan-mode
         ;; spinner
         ;; mode-icons
         ;; input method
         ;; eim
         ;; clipboard
         ;; xclip
         ;; Info & help & documentation
         info+
         apropos+
         ;;; multiple major modes
         mmm-mode ; mumamo-noweb
         polymode
         ;; shell
         readline-complete
         ;; eshell-manual
         shelldoc
         ;; insert-shebang
         ;; others
         ;; guru-mode
         ;; buffer & window, frame
         window-number window-layout ; switch-window
         workgroups2
         ;; e2wm ne2wm
         ;; perspective
         window-purpose
         golden-ratio
         popwin ; shackle
         zoom-window
         ;; minibuffer
         eldoc-eval
         ;; edit
         undo-tree
         multiple-cursors iedit
         expand-region
         edit-server ; edit-server-htmlize
         ;; unicode
         ucs-utils
         ;; large file
         vlf
         ;; kill-ring-search
         ;; kill-ring-ido
         ;; predictive
         ;; spell
         ;; flyspell flyguess ; flyspell-guess inflections
         ;; auto-dictionnary
         ;; dictionary & translation
         define-word
         ;; babel
         ;; imenu
         ;; imenu-anywhere
         ;; jump
         ace-jump-mode ; ace-isearch
         ;; bookmark, register, macro,
         bm
         ;; keybinding
         bind-key
         guide-key ; guide-key-tip
         hydra
         ;; Dired
         ;; direx
         ;; dired-k ; stripe-buffer
         ;; Image
         ;; image+ image-dired+
         ;; PDF
         ;; pdf-tools
         ;; search
         ;; isearch+
         anzu
         highlight-symbol
         visual-regexp visual-regexp-steroids ace-jump-mode
         swiper
         ack-and-a-half ; full-ack
         ag helm-ag
         ;; (m)occur
         ;; ioccur joccur occur-x
         ;; color-moccur moccur-edit
         ;; awk-it
         ;; Org-mode
         org-mode
         org-fstree org-bullets
         ob-julia ob-go ob-prolog ob-mongo ob-http ob-browser
         cdlatex-mode
         ;; org-ac
         org-screenshot org-download
         org-pomodoro
         ;; org-doing
         ;; orgit ; org-magit ; org-linkany
         org-publish ; org2blog ; org-blog org-website
         jekyll-el org2jekyll ; org-jekyll jekyll-modes org-protocol-jekyll
         ;; org-present org-tree-slide org-html5presentation org-impress-js
         org-passwords
         ;; org-magit ; org-linkany
         ;; org-trello
         org-projectile
         ;; Wiki
         ;; oddmuse yaoddmuse org-oddmuse oddmuse-curl
         ;; SpeedReading - OpenSpritz
         spray
         ;; speedread
         ;; help
         ;; discover
         ;; Browser
         ;; eww ; webkit ; w3m
         ;; Email
         ;; mu
         mu4e mu-cite
         ;; notmuch
         ;; BBDB
         bbdb bbdb-vcard
         ;; IRC
         ;; erc erc-highlight-nicknames erc-nick-notify bbdb2erc
         ;; weechat
         ;; RSS
         ;; elfeed
         ;; Podcast & Screencast
         emms helm-emms
         emms-netease emms-player-mpv emms-info-mediainfo
         ;; tools
         ;; calfw sauron ; appt
         ;; gist helm-gist
         yagist
         ;; emms ; emms-get-lyrics
         ;; ampc
         ;; subprocess
         ;; dizzee
         ;; Programming
         ;; code
         ;; glasses-mode
         ;; indent
         ;; highlight-indentation
         indent-guide
         aggressive-indent-mode
         ;; auto-indent-mode clean-aindent
         ;; completion & comprehension
         ;; emacs-ycmd
         ;; lint
         flycheck
         ;; flycheck-tip
         flycheck-pos-tip
         ;; comment
         fic-mode
         poporg ; outorg
         ;; License
         xlicense
         ;; electric
         paredit
         smartparens
         ;; autopair
         rainbow-mode
         rainbow-delimiters
         ;; rainbow-block
         ;; rainbow-identifiers
         ;; document, API, docsets
         dash helm-dash
         ;; RFC
         ;; rfc irfc
         ;; Snippet
         yasnippet
         yasnippet-snippets
         ;; Template
         ;; template file-template
         yatemplate
         ;; Tags
         cscope ascope
         helm-cscope ; xcscope ascope
         gtags helm-gtags ggtags xgtags
         ;; code browser
         ;; sr-speedbar
         ;; neotree
         ;; project-explorer
         ;; compile
         smart-compile smart-compile+
         ;; refactor
         ;; emacs-refactor
         ;; sourcemap -- https://github.com/syohex/emacs-sourcemap
         ;; vcs
         git-modes git-emacs ; git-status
         magit ; magithub
         magit-filenotify magit-gitflow
         magit-gh-pulls
         magit-gerrit
         git-gutter git-gutter+ ; git-gutter-fringe
         ;; git-timemachine
         ;; egg
         mo-git-blame
         diffview
         helm-open-github
         ;; project
         projectile
         helm-projectile ; helm-project
         vagrant vagrant-tramp
         ;; Bug Track System
         ;; bts bts-github
         ;; code assistant
         howdoi
         ;; languages
         ;; Ruby
         ruby-mode
         enh-ruby-mode
         ;; ruby-block
         ruby-hash-syntax ruby-tools
         yari
         ;; auto-complete-ruby ; (conflict with robe-mode?)
         ruby-compilation
         inf-ruby pry
         rbenv ; chruby ; rvm
         ;; rcodetools
         robe-mode helm-robe
         rake rdoc-mode
         yard-mode ; omniref ; helm-rb
         ;; ruby-hash-syntax
         ruby-test-mode ; ruby-test
         rspec-mode ; minitest
         ;; cucumber feature-mode
         ;; rcov-overlay
         rdebug
         motion-mode
         ;; Lisp
         slime
         elisp-slime-nav eldoc-eval
         profile-lisp
         slime-company ; ac-slime
         geiser ; ac-geiser
         ;; sly sly-company ; ac-sly
         eval-sexp-fu
         ;; Scheme
         ;; Clojure
         clojure-mode cider clj-refactor
         cider-eval-sexp-fu align-cljlet
         typed-clojure-mode
         ;; Python
         ;; python python-mode
         jedi ; (depends on auto-complete)
         ;; jedi-core
         company-jedi
         anaconda-mode company-anaconda
         ;; elpy
         pydoc helm-pydoc
         helm-ipython
         ;; C family languages (C, C++, Go, D, F, Rust)
         ;; c-eldoc eassist
         irony-mode
         company-irony ; ac-irony
         irony-eldoc flycheck-irony
         ;; company-c-headers
         ;; auto-complete-clang auto-complete-c-headers
         ;; auto-complete-clang-objc
         ;; bison-mode
         function-args
         ;; Go
         go-mode ; go-eldoc
         ;; go-company
         ;; go-autocomplete ; gocode
         ;; D
         ;; d-mode
         ;; C++
         ;; Rust
         ;; rust-mode
         ;; Lua
         ;; lua-mode
         ;; Swift
         swift-mode
         ;; Web
         web-mode
         skewer-mode ; live web development in Emacs (HTML, CSS, JavaScript)
         ;; emacs-moz-controller ; moz-repl
         ;; JavaScript
         ;; js-mode
         ;; js2-mode
         js3-mode
         tern company-tern ; tern-auto-complete
         swank-js
         ;; ac-js2
         ;; HTML
         htmlize html5
         ;; ac-html
         zencoding-mode
         ;; CSS
         ;; FIXME: can't install css-mode
         ;; css-mode
         ;; auto-complete-css
         css-eldoc
         showcss-mode
         less-css-mode skewer-less
         ;; CoffeeScript
         ;; coffee-mode
         ;; XML
         ;; xml-parse xml-rpc xml-gen
         ;; auto-complete-nxml
         ;; JSON
         json-mode json-reformat json-snatcher
         ;; Markdown
         markdown-mode markdown-preview-mode
         ;; YAML
         yaml-mode
         ;; TeX
         ;; auctex
         ;; company-auctex ; auto-complete-auctex
         ;; auto-complete-latex
         ;; company-math ; ac-math
         ;; reftex
         ;;; Haskell
         ;; haskell-mode
         ;; Erlang
         ;; erlang-mode
         ;; Prolog
         ;; prolog-el
         ;; Verilog
         ;; verilog-mode
         ;; auto-complete-verilog
         ;; Java
         eclim
         javadoc-lookup ; javadoc-help
         ;; java-complete
         ess company-ess
         ;; Julia
         ;; julia-mode
         ;; R
         ;; ac-R
         ;; gnuplot
         ;; gnuplot-mode
         ;; Octave
         ;; ac-octave
         ;; Database
         ;; SQL
         sqlup-mode
         ;; FIXME: depend on auto-complete.
         edbi company-edbi edbi-sqlite edbi-database-url
         ;; NoSQL
         ;; MongoDB
         ;; inf-mongo
         ;; Redis
         ;; eredis
         ;; Assembly
         ;; iasm-mode
         ;; Hex
         ;; hexview-mode
         ;; Frameworks
         ;; Ruby on Rails
         projectile-rails rails-new ; helm-rails
         rhtml-mode ; nxhtml
         ;; yasnippet-rails
         ;; sass-mode haml-mode slim-mode
         ;; emacs-rails ; https://github.com/tomtt/emacs-rails
         ;; Arduino
         ;; arduino-mode
         ;; Android
         ;; android-mode
         ;; Nginx
         nginx-mode
         ;; Linux
         systemd-mode
         xrdb-mode
         ;; vimrc-mode
         ;; crontab-mode
         ;; Arch
         pkgbuild-mode
         ;; aurel
         ;; fvwm-mode glsl-mode
         ;; Windows
         ;; batch-mode
         ;; PowerShell-Mode
         ;; fvwm-mode glsl-mode
         ;; English
         ;; Japanese
         ;; migemo helm-migemo
         ;; Speak
         ;; festival
         ;; Screenshot & Screencast
         screenshot
         capture camcorder
         ;; Libraries
         ;; ibus
         ;; http httpcode httprepl web ; web-beautify
         ;; xpath xpath-parser
         ;; web-server
         restclient company-restclient ; know-your-http-well
         ;; Platforms
         ;; heroku
         ;; wolfram-mode
         ;; Toy
         ;; oniisama
         ;; Emacs keyboard commands log
         command-log-mode
         ;; Integrate into Emacs
         ;; sx ; Stack Exchange
         )
       (mapcar 'el-get-source-name el-get-extra-sources)))

(el-get 'sync my:el-get-packages)



(provide 'init-my-pm-el-get)

;;; init-my-pm-el-get.el ends here
