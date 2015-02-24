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
         ;; benchmark
         ;; benchmark-init
         ;; package
         ;; paradox
         ;; popup
         popup
         pos-tip
         popup-pos-tip
         showtip
         tooltip-help
         ;; Emacs
         auto-compile
         ;; completion
         ;; ido-vertical-mode ido-ubiquitous smex
         helm
         helm-helm-commands helm-descbinds ; helm-themes
         ;; FIXME helm-c-moccur
         helm-cmd-t
         helm-c-yasnippet helm-c-flycheck
         helm-gtags helm-yaetags
         ;; helm-pydoc
         ;; helm-ls-git
         helm-gist
         helm-mu
         ;; helm-delicious
         ;; helm-dictionary
         ;; auto-complete
         ;; auto-complete-yasnippet auto-complete-chunk
         ;; auto-complete-etags
         ;; auto-complete-emacs-lisp
         ;; auto-complete-pcmp
         ;; ac-capf
         company-mode
         ;; ac-helm
         ;; ac-company
         ;; color theme
         color-theme-solarized
         ;; color-theme-almost-monokai
         ;; monokai-theme
         ;; apperance
         ;; emacs-powerline
         highlight-symbol
         ;; cursor-chg
         ;; yascroll
         ;; minimap sublimity
         pretty-mode-plus page-break-lines
         ;; mode-line
         diminish nyan-mode
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
         ;; polymode ; https://github.com/vspinu/polymode
         ;; shell
         ;; readline-complete
         eshell-manual
         ;; insert-shebang
         ;; others
         ;; guru-mode
         ;; buffer & window, frame
         window-number window-layout ; switch-window
         workgroups2
         ;; e2wm ne2wm
         ;; perspective
         popwin
         zoom-window
         ;; minibuffer
         eldoc-eval
         ;; edit
         undo-tree
         multiple-cursors
         expand-region
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
         ;; babel
         ;; imenu
         imenu-anywhere
         ;; jump
         ace-jump-mode ; ace-isearch
         ;; bookmark, register, macro,
         bm
         ;; keybinding
         bind-key
         guide-key ; guide-key-tip
         hydra
         ;; dired
         ;; direx
         ;; dired-k ; stripe-buffer
         ;; search
         ;; isearch+
         anzu
         visual-regexp visual-regexp-steroids ace-jump-mode
         ack-and-a-half ; full-ack
         ag helm-ag
         ;; awk-it
         ;; Org-mode
         org-mode
         org-fstree org-bullets
         ob-julia
         ;; org-ac
         org-screenshot
         org-pomodoro
         ;; org-doing
         ;; orgit ; org-magit ; org-linkany
         org-publish org-jekyll jekyll-el ;; org-blog org-website
         ;; org-present org-tree-slide org-html5presentation org-impress-js
         org-passwords
         ;; org-magit ; org-linkany
         org-publish org-jekyll jekyll-el
         ;; org-trello
         outorg
         ;; Wiki
         ;; oddmuse yaoddmuse org-oddmuse oddmuse-curl
         ;; SpeedReading - OpenSpritz
         spray
         ;; speedread
         ;; help
         ;; discover
         ;; Email
         ;; mu
         mu4e mu-cite
         ;; BBDB
         bbdb ; bbdb- ; FIXME: bbdb-vcard
         ;; IRC
         erc erc-highlight-nicknames erc-nick-notify bbdb2erc
         ;; RSS
         elfeed
         ;; tools
         ;; calfw sauron ; appt
         gist
         ;; emms ; emms-get-lyrics
         ;; ampc
         ;; subprocess
         dizzee
         ;; Programming
         ;; code
         ;; glasses-mode
         ;; indent
         ;; highlight-indentation
         indent-guide
         aggressive-indent-mode
         ;; auto-indent-mode clean-aindent
         ;; complete
         ;; emacs-ycmd
         ;; lint
         flycheck
         ;; flycheck-tip
         flycheck-pos-tip
         ;; comment
         fic-mode
         poporg
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
         ;; snippet
         yasnippet
         ;; Tags
         cscope ascope
         helm-cscope ; xcscope ascope
         gtags helm-gtags ggtags xgtags ; use program global
         ;; code browser
         sr-speedbar
         ;; neotree
         ;; project-explorer
         ;; compile
         smart-compile smart-compile+
         ;; refactor
         emacs-refactor
         ;; sourcemap -- https://github.com/syohex/emacs-sourcemap
         ;; vcs
         git-modes git-emacs ; git-status
         magit ; magithub
         magit-filenotify magit-gitflow
         magit-gh-pulls
         magit-gerrit
         git-gutter git-gutter+ ; git-gutter-fringe
         ; git-timemachine
         ;; egg
         mo-git-blame
         diffview
         ;; project
         projectile
         helm-project
         vagrant vagrant-tramp
         ;; bug track system
         bts bts-github
         ;; code assistant
         howdoi
         ;; languages
         ;; Ruby
         ruby-mode
         ;; enh-ruby-mode
         ;; ruby-block
         yari
         ;; auto-complete-ruby ; (conflict with robe-mode?)
         inf-ruby ruby-compilation rvm ;; pry
         ;; company-inf-ruby
         ;; rcodetools
         robe-mode helm-robe
         rake
         rspec-mode
         yard-mode
         ruby-test-mode ; ruby-test
         ;; ruby-hash-syntax
         ;; cucumber feature-mode
         ;; Lisp
         slime
         elisp-slime-nav eldoc-eval
         profile-lisp
         slime-company ; ac-slime
         geiser ; ac-geiser
         ;; Scheme
         ;; Clojure
         clojure-mode cider
         ;; ac-cider ; company-cider
         ;; Python
         python python-mode
         ;; jedi
         helm-ipython
         ;; C family languages (C, C++, Go, D, F, Rust)
         c-eldoc eassist
         irony-mode
         company-irony ; ac-irony
         company-c-headers
         ;; auto-complete-clang auto-complete-c-headers
         ;; auto-complete-clang-objc
         ;; bison-mode
         ;; Go
         go-mode go-eldoc
         go-company
         ;; go-autocomplete ; gocode
         ;; D
         ;; d-mode
         ;; C++
         ;; Rust
         ;; rust-mode
         ;; Lua
         ;; lua-mode
         ;; Web
         web-mode
         skewer-mode ; live web development in Emacs (HTML, CSS, JavaScript)
         emacs-moz-controller ; moz-repl
         ;; JavaScript
         ;; js-mode
         ;; js2-mode
         js3-mode
         swank-js
         ;; ac-js2
         tern ; company-tern
         ;; HTML
         htmlize html5
         ;; ac-html
         ;; CSS
         css-mode
         ;; auto-complete-css
         css-eldoc
         showcss-mode
         ;; CoffeeScript
         ;; coffee-mode
         ;; XML
         ;; auto-complete-nxml
         ;; JSON
         json-mode json-reformat json-snatcher
         ;; Markdown
         markdown-mode markdown-preview-mode
         ;; YAML
         yaml-mode
         ;; TeX
         auctex
         company-auctex ; auto-complete-auctex
         ;; auto-complete-latex
         company-math ; ac-math
         ;; reftex
         ;;; Haskell
         ;; haskell-mode
         ;; Erlang
         erlang-mode
         ;; Prolog
         prolog-el
         ;; Verilog
         ;; verilog-mode
         ;; auto-complete-verilog
         ;; Java
         javadoc-lookup ; javadoc-help
         eclim
         ;; java-complete
         ess
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
         edbi company-edbi
         ;; NoSQL
         ;; MongoDB
         ;; inf-mongo
         ;; Redis
         ;; eredis
         ;; Assembly
         iasm-mode
         ;; Hex
         hexview-mode
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
         ;; vimrc-mode
         ;; crontab-mode
         ;; Arch
         pkgbuild-mode
         ;; fvwm-mode glsl-mode
         ;; Windows
         ;; batch-mode
         ;; PowerShell-Mode
         ;; fvwm-mode glsl-mode
         ;; English
         ;; Japanese
         migemo helm-migemo
         ;; Speak
         festival
         ;; Screenshot & Screencast
         screenshot
         capture camcorder
         ;; Toy
         ;; oniisama
         ;; Emacs keyboard commands log
         command-log-mode
         ;; Integrate into Emacs
         sx ; Stack Exchange
         )
       (mapcar 'el-get-source-name el-get-extra-sources)))

(el-get 'sync my:el-get-packages)



(provide 'init-my-pm-el-get)

;;; init-my-pm-el-get.el ends here
