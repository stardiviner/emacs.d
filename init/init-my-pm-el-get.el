;;; init-my-pm-el-get.el --- init el-get
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

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
         helm-descbinds helm-project helm-ls-git helm-c-yasnippet
         helm-rails helm-pydoc
         helm-gist
         auto-complete
         auto-complete-yasnippet auto-complete-chunk
         auto-complete-etags
         auto-complete-emacs-lisp
         auto-complete-pcmp
         company-mode
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
         ;; input method
         ;; eim
         ;; Info & help & documentation
         info+
         discover-my-major
         ;;; multiple major modes
         mmm-mode ; mumamo-noweb
         ;; shell
         eshell-manual
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
         ;; kill-ring-search
         kill-ring-ido
         ;; predictive
         ;; spell
         flyspell flyguess ; flyspell-guess inflections
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
         dired-k ; stripe-buffer
         ;; search
         isearch+
         anzu
         visual-regexp visual-regexp-steroids ace-jump-mode
         ack-and-a-half
         ;; Org-mode
         org-mode
         org-fstree org-bullets
         org-ac
         org-magit ; org-linkany
         org-publish org-jekyll jekyll-el
         ;; help
         ;; discover
         ;; Email
         ;; mu mu-cite
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
         highlight-indentation
         ;; lint
         flycheck
         ;; comment
         fic-mode
         ;; electric
         paredit
         smartparens
         ;; autopair
         rainbow-mode rainbow-delimiters
         ;; document, API, docsets
         helm-dash
         ;; RFC
         irfc
         ;; snippet
         yasnippet
         ;; Tags
         cscope ; xcscope
         ascope
         ;; code browser
         sr-speedbar
         neotree
         project-explorer
         ;; compile
         smart-compile smart-compile+
         ;; vcs
         git-modes git-emacs ; git-status
         magit ; magithub
         git-gutter git-gutter-fringe ; git-gutter+
         git-timemachine
         ;; egg
         mo-git-blame
         ;; project
         projectile
         ;; languages
         ;; Ruby
         ruby-mode
         enh-ruby-mode
         ruby-block
         yari
         auto-complete-ruby
         inf-ruby rcodetools ruby-compilation rvm
         pry
         robe-mode
         rspec-mode
         yard-mode
         ruby-refactor
         ruby-hash-syntax
         ;; Lisp
         slime
         elisp-slime-nav elisp-format eldoc-eval
         ;; ac-slime slime-company
         geiser
         ac-geiser
         ;; Scheme
         ;; Clojure
         clojure-mode cider ac-nrepl ; company-cider
         ;; Python
         python ; python-mode
         jedi
         ;; C family languages
         auto-complete-clang auto-complete-c-headers
         c-eldoc eassist
         irony-mode ac-irony company-irony
         ;; Go
         go-mode go-eldoc
         go-autocomplete ; gocode
         ;; D
         d-mode
         ;; C++
         ;; Lua
         lua-mode
         ;; Web
         web-mode
         ;; JavaScript
         ;; js-mode
         ;; js2-mode
         js3-mode
         swank-js
         ;; HTML
         ;; CSS
         auto-complete-css
         ;; css-mode
         css-eldoc
         ;; XML
         auto-complete-nxml
         ;; JSON
         json-mode json-reformat json-snatcher
         ;; Markdown
         markdown-mode
         ;; YAML
         yaml-mode
         ;; TeX
         ;; auctex auto-complete-auctex
         ;; auto-complete-latex
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
         ess
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
         crontab-mode
         ;; Database
         ;; SQL
         sqlup-mode
         )
       (mapcar 'el-get-source-name el-get-extra-sources)))

(el-get 'sync my-el-get-packages)



(provide 'init-my-pm-el-get)

;;; init-my-pm-el-get.el ends here
