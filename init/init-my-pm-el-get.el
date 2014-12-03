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
         ;; popup
         popup
         pos-tip
         popup-pos-tip
         showtip
         tooltip-help
         ;; Emacs
         ;; completion
         ;; ido-vertical-mode ido-ubiquitous smex
         helm
         helm-helm-commands helm-descbinds ; helm-themes
         helm-cmd-t ; helm-ls-git
         helm-c-yasnippet helm-c-flycheck
         helm-gtags helm-yaetags ; helm-c-moccur
         helm-rails helm-pydoc
         helm-gist
         helm-mu
         ;; helm-delicious
         helm-dictionary
         ;; helm-dictionary
         auto-complete
         auto-complete-yasnippet auto-complete-chunk
         auto-complete-etags
         auto-complete-emacs-lisp
         auto-complete-pcmp
         ac-capf
         company-mode
         ac-helm
         ;; ac-company
         ;; color theme
         color-theme-solarized
         ;; color-theme-almost-monokai
         ;; monokai-theme
         ;; apperance
         highlight-symbol
         ;; cursor-chg
         ;; yascroll
         ;; minimap
         ;; modeline
         diminish pretty-mode page-break-lines
         ;; mode-icons
         ;; input method
         ;; eim
         ;; clipboard
         ;; xclip
         ;; Info & help & documentation
         info+
         discover-my-major
         ;;; multiple major modes
         mmm-mode ; mumamo-noweb
         ;; shell
         ;; readline-complete
         eshell-manual
         insert-shebang
         ;; others
         ;; guru-mode
         ;; buffer & window, frame
         window-number ; switch-window
         workgroups2 ; e2wm
         popwin
         ;; minibuffer
         eldoc-eval
         ;; edit
         undo-tree
         multiple-cursors
         ;; large file
         vlf
         ;; kill-ring-search
         ; kill-ring-ido
         ;; predictive
         ;; spell
         flyspell flyguess ; flyspell-guess inflections
         ;; auto-dictionnary
         ;; dictionary & translation
         babel
         ;; imenu
         ;; jump
         ace-jump-mode ; ace-isearch
         ;; bookmark, register, macro,
         bm
         ;; keybinding
         bind-key
         guide-key ; guide-key-tip
         ;; dired
         ;; direx
         ;; dired-k ; stripe-buffer
         ;; search
         isearch+
         anzu
         visual-regexp visual-regexp-steroids ace-jump-mode
         ack-and-a-half ; full-ack
         ag helm-ag
         ;; awk-it
         ;; Org-mode
         org-mode
         org-fstree org-bullets
         ;; org-ac
         org-magit ; org-linkany
         org-publish org-jekyll jekyll-el
         ;; org-trello
         org-passwords
         poporg
         ;; Wiki
         ;; SpeedReading - OpenSpritz
         spray
         ;; speedread
         ;; help
         ;; discover
         ;; Email
         ;; mu
         mu-cite
         ;; BBDB
         bbdb ; bbdb-vcard
         ;; tools
         calfw sauron ; appt
         gist
         emms ; emms-get-lyrics
         ;; Programming
         ;; code
         ;; glasses-mode
         ;; indent
         ;; highlight-indentation
         indent-guide
         aggressive-indent-mode
         ;; auto-indent-mode clean-aindent
         ;; lint
         flycheck
         ;; flycheck-tip
         flycheck-pos-tip
         ;; comment
         fic-mode
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
         rfc irfc
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
         ;; sourcemap -- https://github.com/syohex/emacs-sourcemap
         ;; vcs
         git-modes git-emacs ; git-status
         magit ; magithub
         git-gutter git-gutter+ ; git-gutter-fringe
         git-timemachine
         ;; egg
         mo-git-blame
         ;; project
         projectile
         helm-project
         vagrant vagrant-tramp
         ;; code assistant
         howdoi
         ;; languages
         ;; Ruby
         ruby-mode
         enh-ruby-mode
         ruby-block
         yari
         ;; auto-complete-ruby ; (conflict with robe-mode?)
         inf-ruby rcodetools ruby-compilation rvm
         pry
         robe-mode helm-robe
         rspec-mode
         yard-mode
         ruby-refactor
         ruby-hash-syntax
         ;; Lisp
         slime
         elisp-slime-nav elisp-format eldoc-eval
         ;; slime-company
         ac-slime
         geiser
         ac-geiser
         ;; Scheme
         ;; Clojure
         clojure-mode cider ; ac-nrepl
         ac-cider
         ;; company-cider
         ;; Python
         python python-mode
         ;; jedi
         ;; C family languages (C, C++, Go, D, F, Rust)
         c-eldoc eassist
         irony-mode ac-irony ; company-irony
         ;; company-c-headers
         auto-complete-clang auto-complete-c-headers
         auto-complete-clang-objc
         ;; Go
         go-mode go-eldoc
         ;; go-autocomplete ; gocode
         ;; D
         d-mode
         ;; C++
         ;; Rust
         ;; Lua
         lua-mode
         ;; Web
         web-mode
         ;; JavaScript
         ;; js-mode
         ;; js2-mode
         js3-mode
         swank-js
         ac-js2
         tern ; company-tern
         ;; HTML
         ;; CSS
         css-mode
         auto-complete-css
         css-eldoc
         showcss-mode
         ;; CoffeeScript
         coffee-mode
         ;; XML
         auto-complete-nxml
         ;; JSON
         json-mode json-reformat json-snatcher
         ;; Markdown
         markdown-mode
         ;; YAML
         yaml-mode
         ;; TeX
         ;; auctex
         ;; reftex
         ;; company-auctex
         auto-complete-auctex
         auto-complete-latex
         ;; ac-math
         ;;; Haskell
         haskell-mode
         ;; Verilog
         verilog-mode
         auto-complete-verilog
         ;; Java
         javadoc-lookup ; javadoc-help
         ;; java-complete
         ;; R
         ;; ac-R
         ;; ess
         ;; gnuplot
         gnuplot-mode
         ;; Octave
         ac-octave
         ;; Frameworks
         ;; Ruby on Rails
         projectile-rails ;; rinari
         rhtml-mode ; nxhtml
         ;; emacs-rails ; https://github.com/tomtt/emacs-rails
         ;; Arduino
         arduino-mode
         ;; Android
         ;; android-mode
         ;; Nginx
         nginx-mode
         ;; Linux
         ;; Arch
         pkgbuild-mode
         ;; Database
         ;; SQL
         sqlup-mode
         ;; NoSQL
         ;; MongoDB
         ;; Redis
         ;; Assembly
         iasm-mode
         ;; English
         ;; Japanese
         ;; migemo helm-migemo
         ;; Toy
         ;; oniisama
         )
       (mapcar 'el-get-source-name el-get-extra-sources)))

(el-get 'sync my:el-get-packages)



(provide 'init-my-pm-el-get)

;;; init-my-pm-el-get.el ends here
