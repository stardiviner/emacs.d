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
         ;; apperance
         highlight-symbol
         cursor-chg
         minimap
         ;; modeline
         diminish pretty-mode page-break-lines
         ;; Info & help & documentation
         info+
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
         ;; spell
         flyspell flyspell-guess
         ;; predictive
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
         ;; FIXME: bbdb ; bbdb-vcard
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
         ;; RFC
         irfc
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
         ruby-mode
         ;; ruby-electric (merged into Ruby)
         ruby-block
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
         auctex auto-complete-auctex
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
         ac-octave
         ;; Frameworks
         ;; Arduino
         arduino-mode
         )
       (mapcar 'el-get-source-name el-get-extra-sources)))

(el-get 'sync my-el-get-packages)



(provide 'init-my-pm-el-get)

;;; init-my-pm-el-get.el ends here
