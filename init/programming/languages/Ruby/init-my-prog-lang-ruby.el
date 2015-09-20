;;; init-my-prog-lang-ruby.el --- my Ruby init

;;; Commentary:


;;; Code:


(if (featurep 'enh-ruby-mode)
    (lambda ()
      (add-to-list 'auto-mode-alist '("\\.rb\'" . enh-ruby-mode))
      (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
      )
  (add-to-list 'auto-mode-alist '("\\.rb\'" . ruby-mode))
  (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
  )

;; Gemfile, Capfile, Rakefile
(add-to-list 'auto-mode-alist
             '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . enh-ruby-mode))
;; irb(irbrc), pry(pryrc), gem(gemspec, gemrc), rackup(ru), Thor(thor),
(add-to-list 'auto-mode-alist
             '("\\.\\(?:gemspec\\|irbrc\\|pryrc\\|gemrc\\|rake\\|ru\\|thor\\)\\'" . enh-ruby-mode))

;; We never want to edit Rubinius bytecode or MacRuby binaries
(add-to-list 'completion-ignored-extensions ".rbc")
(add-to-list 'completion-ignored-extensions ".rbo")

;; highlight symbol: dot .
(font-lock-add-keywords
 'enh-ruby-mode
 '(("[[:alnum:]]\\(\\.\\)[[:alnum:]]"
    (1 '(:foreground "red" :weight 'bold))
    )))

;;; custom functions

;;; FIXME: key binding always override by flyspell-auto-correct-word.
(defun insert-arrow ()
  "Insert => for Ruby old school style hash."
  (interactive)
  (delete-horizontal-space t)
  (insert " => "))

;; (deprecated) replaced by package `ruby-tools'.
;; (defun insert-ruby-interpolate ()
;;   "In a double quoted string, interpolate."
;;   (interactive)
;;   (insert "#")
;;   (when (and
;;          (looking-back "\".*" nil)
;;          (looking-at ".*\""))
;;     (insert "{}")
;;     (backward-char 1)))


;;; [ ruby-mode ]

;; - [C-M-n] -- end of block
;; - [C-M-p] -- beginning of block
;; - [C-M-q] -- (prog-indent-sexp)
;;
;; - [C-c C-r] -- (ruby-send-region)
;; - [C-c C-b] -- (ruby-send-block)
;; - [C-c C-x] -- (ruby-send-definition)
;; - [C-c C-l] -- (ruby-load-file)
;; - [C-x C-e] -- (ruby-send-last-sexp)
;; - [C-c C-s] -- (inf-ruby)
;; - [C-c C-z] -- (ruby-switch-to-inf)


(setq ruby-use-smie t       ; use sexp navigation for Ruby
      ;; bellowing effect only when `ruby-use-smie' is `nil'.
      ruby-deep-indent-paren '(?\( ?\[ ?\] t)
      ruby-deep-indent-paren-style 'space)


;;; [ enh-ruby-mode ] --

(use-package enh-ruby-mode
  :init
  (setq enh-ruby-bounce-deep-indent t
        enh-ruby-check-syntax 'errors-and-warnings
        enh-ruby-comment-column 32
        enh-ruby-deep-arglist t
        enh-ruby-deep-indent-paren t
        ;; enh-ruby-deep-indent-paren-style
        ;; enh-ruby-extra-keywords
        enh-ruby-indent-level 2
        ;; enh-ruby-indent-tabs-mode nil
        enh-ruby-hanging-indent-level 2
        enh-ruby-hanging-brace-indent-level 2
        enh-ruby-hanging-brace-deep-indent-level 0
        enh-ruby-hanging-paren-indent-level 2
        enh-ruby-hanging-paren-deep-indent-level 0
        enh-ruby-use-encoding-map t
        enh-ruby-use-ruby-mode-show-parens-config t
        enh-ruby-add-encoding-comment-on-save t ; add ruby magic encoding comment on save.
        )

  :config
  (unless (derived-mode-p 'prog-mode)
    (run-hooks 'prog-mode-hook))

  ;; TODO: modify those colors.
  ;; Words prefixed with $ are global variables,
  ;; prefixed with @ are instance variables.
  (modify-syntax-entry ?$ "w") ; global variable
  (modify-syntax-entry ?@ "w") ; instance variable
  ;; FIXME:
  ;; (modify-syntax-entry ?@@ "w") ; class variable
  (modify-syntax-entry ?? "w")
  (modify-syntax-entry ?! "w")
  (modify-syntax-entry ?: ".")

  (erm-define-faces)
  (set-face-attribute 'enh-ruby-op-face nil
                      :foreground "red")
  (set-face-attribute 'enh-ruby-string-delimiter-face nil
                      :foreground "orange")
  (set-face-attribute 'enh-ruby-regexp-delimiter-face nil
                      :foreground "dark magenta")
  (set-face-attribute 'enh-ruby-regexp-face nil
                      :foreground "cyan")
  (set-face-attribute 'enh-ruby-heredoc-delimiter-face nil
                      :foreground "dark green")

  (set-face-attribute 'erm-syn-warnline nil
                      :box '(:color "orange" :line-width -1))
  (set-face-attribute 'erm-syn-errline nil
                      :box '(:color "red" :line-width -1))

  
  (electric-indent-local-mode 1)
  (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
  ;; (define-key ruby-mode-map (kbd "TAB") 'indent-for-tab-command)
  

  (define-key enh-ruby-mode-map (kbd "C-c C-'") 'insert-arrow)
  ;; (define-key enh-ruby-mode-map (kbd "#") 'insert-ruby-interpolate)

  ;; hs-minor-mode (hide-show)
  ;; (add-to-list 'hs-special-modes-alist
  ;;              '(ruby-mode
  ;;                "\\(class\\|def\\|do\\|if\\)" "\\(end\\)" "#"
  ;;                (lambda (arg) (ruby-end-of-block)) nil))

  )


;;; [ ruby-hash-syntax ] -- automatically convert the selected region of ruby code between 1.8 and 1.9 hash styles.

;;; Usage:
;;
;; - region + `ruby-toggle-hash-syntax'

(define-key my-prog-code-map (kbd "c") 'ruby-toggle-hash-syntax) ; convert


;;; [ ruby-tools ] -- Ruby tools is a collection of handy functions for Emacs ruby-mode.

;;; Ruby tools is a collection of handy functions for Emacs ruby-mode. You can
;;; turn a string to symbol, symbol to string, single to double quote string,
;;; double to single quote string, clear string, interpolate and more...

;;; Usage:
;;
;; (the | represents the point position)
;; - [C-'] :: convert symbol -> string, e.g. foo(|:bar)
;; - [C-:] :: convert string -> symbol, e.g. foo(|'bar')
;; - [C-"] :: convert single quote string to double quote string.
;; - [C-'] :: convert double quote string to single quote string.
;; - [C-;] :: clear string content
;; - [#]   :: string interpolation

(add-hook 'ruby-mode-hook 'ruby-tools-mode)
(add-hook 'enh-ruby-mode-hook 'ruby-tools-mode)


;;; [ ruby-block ] -- highlight matching block

;; (require 'ruby-block)
;;
;; (setq ruby-block-delay 0)
;; (setq ruby-block-highlight-toggle t)
;; (ruby-block-mode t)


;;; [ ruby-electric ]

(add-hook 'ruby-mode-hook 'ruby-electric-mode)
(add-hook 'enh-ruby-mode-hook 'ruby-electric-mode)


;;; [ ruby-end ]

;; (add-hook 'ruby-mode-hook 'ruby-end-mode)
;; (add-hook 'enh-ruby-mode-hook 'ruby-end-mode)



(add-to-list 'which-func-modes 'ruby-mode)


;;; [ yard-mode ] -- for Ruby YARD comments

(add-hook 'ruby-mode-hook 'yard-mode)
(add-hook 'enh-ruby-mode-hook 'yard-mode)

;; If you would also like eldoc support, so that the expected syntax for the tag
;; beneath your cursor is displayed in the minibuffer, add that hook too:
(add-hook 'ruby-mode-hook 'eldoc-mode)


;;; [ yari ] -- Yet Another Ri Interface

;; yari.el provides an Emacs frontend to Ruby's `ri' documentation tool. It offers lookup and completion.

(dolist (hook '(ruby-mode-hook
                enh-ruby-mode-hook
                ))
  (add-hook hook (lambda ()
                   ;; (setq yari-ruby-program-name "ruby"
                   ;;       yari-ri-program-name "ri")

                   ;; (local-set-key (kbd "C-h d k") 'yari)

                   ;; or with my-prog-help-document-map prefix.
                   (unless (boundp 'ruby-help-doc-map)
                     (define-prefix-command 'ruby-help-doc-map))
                   (local-set-key (kbd "C-h d") 'ruby-help-doc-map)
                   (if (featurep 'helm)
                       (define-key ruby-help-doc-map (kbd "k") 'yari-helm) ; interactive with Helm.
                     (define-key ruby-help-doc-map (kbd "k") 'yari) ; seems minibuffer use ido if ido is enabled.
                     )
                   )))


;;; [ rvm ] -- integrates Emacs with the rvm (Ruby Version Manager)

;;; Usage:
;;; - [M-x rvm-activate-corresponding-ruby]
;;; - [M-x rvm-use] -- to use another ruby version.
;;; - open the source of any rubygem in your current gemset.
;;;   [M-x rvm-open-gem]

;; (require 'rvm)
;;
;; (rvm-use-default)        ; use rvm's default ruby for the current Emacs session.
;; (setq rvm-verbose t)     ; print rvm switching Ruby version message.
;;
;; (dolist (hook '(ruby-mode-hook
;;                 enh-ruby-mode-hook
;;                 ))
;;   (add-hook hook 'rvm-activate-corresponding-ruby))


;;; [ rbenv ] -- use rbenv to manage your Ruby versions within Emacs

;;; Usage:
;;
;; - `global-rbenv-mode' :: activate / deactivate rbenv.el (The current Ruby version is shown in the modeline).
;; - `rbenv-use-global' :: will activate your global Ruby.
;; - `rbenv-use-corresponding' :: will activate your corresponding Ruby.
;; - `rbenv-use' :: allows you to choose what ruby version you want to use.

;;; NOTE: that rbenv.el always configures the complete Emacs session. There is
;;; no way to set the Ruby version on a per buffer basis.

;;; IMPORTANT:: Currently you need to set this variable before you load rbenv.el
(setq rbenv-installation-dir "~/.rbenv/") ; "/usr/local/rbenv"

(setq rbenv-show-active-ruby-in-modeline t
      ;; rbenv-executable "~/.rbenv/bin/rbenv"
      ;; rbenv-binary-paths '((shims-path . "~/.rbenv/shims")
      ;;                      (bin-path . "~/.rbenv/bin"))
      ;; rbenv-ruby-shim "~/.rbenv/shims/ruby"
      rbenv-modeline-function 'rbenv--modeline-plain ; 'rbenv--modeline-with-face
      )

;; (defun rbenv--modeline-with-face (current-ruby)
;;   (append '(" [")
;;           (list (propertize current-ruby 'face 'rbenv-active-ruby-face))
;;           '("]")))

;; (defun rbenv--modeline-plain (current-ruby)
;;   (list " [" current-ruby "]"))

;;; Setting rbenv path
;; (setenv "PATH" (concat (getenv "HOME") "/.rbenv/shims:" (getenv "HOME") "/.rbenv/bin:" (getenv "PATH")))
;; (setq exec-path (cons (concat (getenv "HOME") "/.rbenv/shims") (cons (concat (getenv "HOME") "/.rbenv/bin") exec-path)))

(global-rbenv-mode 1)

(rbenv-use-global)
;; (rbenv-use "2.2.0")


;;; [ inf-ruby ] -- provides a REPL buffer connected to a Ruby(irb/pry) subprocess
;;
;;; Usage:
;;;
;;; - [M-x inf-ruby] :: A simple IRB process can be fired up with M-x inf-ruby.
;;;
;;; - [M-x inf-ruby-console-auto] :: To launch a REPL with project-specific
;;;   console instead, type M-x inf-ruby-console-auto. It recognizes several
;;;   project types, including Rails, gems and anything with racksh in their
;;;   Gemfile.
;;;
;;; - To see the list of the keybindings defined by inf-ruby-minor-mode, type
;;;   M-x describe-function [RET] inf-ruby-minor-mode [RET].
;;
;; - [M-x run-ruby] -- which runs IRB in an Emacs buffer
;; keybindings:
;; - [C-c C-s] -- inf-ruby
;; - [C-c C-z] -- ruby-switch-to-inf
;; - [C-c C-l] -- ruby-load-file :: load current file source code for completion.
;; - [M-C-x]   -- ruby-send-definition
;; - [C-x C-e] -- ruby-send-last-sexp
;; - [C-c C-b] -- ruby-send-block
;; - [C-c M-b] -- ruby-send-block-and-go
;; - [C-c C-x] -- ruby-send-definition
;; - [C-c M-x] -- ruby-send-definition-and-go
;; - [C-c C-r] -- ruby-send-region
;; - [C-c M-r] -- ruby-send-region-and-go
;;
;; - [RET] -- after the end of the process' output sends the text from the end of process to point.
;; - [RET] -- before the end of the process' output copies the sexp ending at point
;;           to the end of the process' output, and sends it.
;; - [DEL] -- converts tabs to spaces as it moves back.
;; - [TAB] -- completes the input at point. IRB, Pry and Bond completion is supported.
;;            Helm is supported at here.
;; - [C-M-q] -- does TAB on each line starting within following expression.
;; - Paragraphs are separated only by blank lines. # start comments.
;; - If you accidentally suspend your process, use comint-continue-subjob to continue it.
;;
;; - [C-x C-q] -- rspec / ruby-compilation

(use-package inf-ruby
  :config
  (add-to-list 'inf-ruby-implementations
               '("inf-ruby" . "irb --inf-ruby-mode --noreadline -EUTF-8"))
  
  (setq inf-ruby-default-implementation "ruby"
        inf-ruby-prompt-read-only t
        )

  ;; integrate with rvm.el
  ;; (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  ;;   (rvm-activate-corresponding-ruby))

  (add-hook 'after-init-hook 'inf-ruby-switch-setup)

  (dolist (hook '(ruby-mode-hook
                  enh-ruby-mode-hook
                  ))
    (add-hook hook
              (lambda ()
                (inf-ruby-minor-mode)
                
                ;; for `company-capf'
                (add-to-list (make-local-variable 'completion-at-point-functions)
                             'inf-ruby-completion-at-point)
                ;; put `robe-complete-at-point' ahead of `inf-ruby-completion-at-point', higher priority.
                (remq 'robe-complete-at-point completion-at-point-functions)
                (setq-local completion-at-point-functions
                            (append '(robe-complete-at-point) completion-at-point-functions))
                )))

  (define-key my-prog-inferior-map (kbd "r a") 'inf-ruby-console-auto)
  (define-key inf-ruby-minor-mode-map (kbd "C-c C-s") 'inf-ruby-console-auto)
  )



;;; [ pry (emacs-pry) ] -- Pry support within Emacs

;;; Usage:
;;
;; -`run-pry' :: starts a Pry REPL inside a modified term-mode buffer.
;; - `pry-intercept' :: command allows for quick debuging into a test without
;;   needing to write the binding.pry into the file.
;; - Automatically shows source in emacs buffer.
;; - You can use the pointer or other emacs commands to move the cursor on the
;;   command line and Pry will be aligned with the new position.
;; Optional
;;
;; - $ gem install pry-nav pry-stack_explorer
;; - $ gem install termios # replaces the running of: stty sane

;; (require 'pry)
;;
;; ;; (setq pry-program-name "pry")           ; program invoked by the `run-pry' command.
;;
;; (define-key my-inferior-ruby-map (kbd "p") 'run-pry)
;; (define-key my-inferior-ruby-map (kbd "C-p") 'pry-intercept)
;; (define-key my-inferior-ruby-map (kbd "C-r") 'pry-intercept-rerun)


;;; [ Robe ] -- Code navigation, documentation lookup and completion for Ruby.
;;
;; Robe is a code assistance tool that uses a Ruby REPL subprocess with your
;; application or gem code loaded, to provide information about loaded classes
;; and modules, and where each method is defined.
;;
;; Generally, you'll want to start with `M-x inf-ruby-console-auto'. If there's
;; no Ruby console running, most interactive commands provided by Robe will
;; offer to launch it automatically.
;;
;; As you change the code in your project, you'll want to update the running
;; process. To load the current file, type `C-c C-l (ruby-load-file)', see
;; inf-ruby for more commands. When you're developing a Rails project, you can
;; type `C-c C-k' instead, to reload the whole environment at once.
;;
;; Features:
;;
;; - Jump to method definition
;; - Jump to `super' or a constructor called at point
;; - Jump to a module or class (provided it has at least one method defined)
;; - Display method documentation
;; - Display information about method called at point using ElDoc
;; - Method and constant name completion
;;
;; To see the available commands, type M-x describe-package RET robe RET.
;;
;;; Usage:
;;
;; start Robe server.
;; 1. [M-x inf-ruby] :: execute this command in a ruby file buffer.
;; 2. [M-x robe-start]
;; 3. Project
;;
;;    To launch a REPL with project-specific console instead, type M-x
;;    `inf-ruby-console-auto'. It recognizes several project types, including
;;    Rails, gems and anything with racksh in their Gemfile.
;;
;; - [C-c C-l] -- `ruby-load-file', to load the current file in your project for completion.
;; - [C-c C-k] -- `robe-rails-refresh', if you're developing a Rails project.
;; - [C-c C-d] -- `robe-doc',
;; - [M-.] -- `robe-jump' (jump to definition)
;; - [M-,] -- pop tag mark
;;
;;; Commands:
;;
;; - `ruby-switch-to-inf' :: switches the current buffer to the ruby process buffer.
;; - `ruby-send-block'[-and-go] :: sends the current block to the ruby process.
;; - `ruby-send-definition'[-and-go] :: sends the current definition to the ruby process.
;; - `ruby-send-region'[-and-go] :: sends the current region to the ruby process.
;;
;;
;; input code in `inf-ruby' buffer.
;;   > Robe.stop # => nil :: stop Robe server.
;; debug Robe log file
;; $ tailf /tmp/robe-access.log
;;
;;; Notes
;;
;; - We can't jump to methods defined in C (such as most of the core
;;   classes). To read their docs, install pry-doc or add it to your Gemfile.
;;
;; - We can't jump to lazily defined methods, such as model.column or find_by_
;;   ActiveRecord methods, before they've been called. This is treatable, but
;;   low priority.
;;
;; - Jumping to methods defined with Module#delegate just brings us to the place
;;   where delegate is called, which is accurate, but often less than useful.
;;
;; - Having more than one inf-ruby buffer at a time is not supported. If you see
;;   unexpected "Method not found" errors, check if you have an older one.
;;
;; - We may get the context wrong for code inside a block if the method it's
;;   passed to uses instance_eval or instance_exec.
;;
;;
;;; Robe server work remotely (in other machine: VirtualBox, vagrant, Remote Host, etc)
;;
;; 1. make `inf-ruby' and `inf-ruby-console-auto' to launch the REPL process
;;    on the remote machine when called from a remote buffer.
;; 2. then you should only have to change Robe settings to point to the remote machine
;;    instead of the localhost. (make sure remote machine port is open)

(setq robe-turn-on-eldoc t
      ;; - t, `completion-at-point' candidates buffer will have constants,
      ;;   methods and arguments highlighted in color.
      ;; - 'nil, to disable ac-robe face property in ac-menu.
      robe-highlight-capf-candidates t
      )

;; start Robe server.
(eval-after-load 'robe
  '(progn
     (inf-ruby)
     (robe-start)))

(dolist (hook '(ruby-mode-hook
                enh-ruby-mode-hook
                inf-ruby-mode-hook
                ))
  (add-hook hook 'robe-mode))

(add-hook 'robe-mode-hook
          (lambda ()
            (local-set-key (kbd "M-.") 'robe-jump)
            (local-set-key (kbd "C-h d d") 'robe-doc)

            (unless (boundp 'ruby-send-to-inferior-map)
              (define-prefix-command 'ruby-send-to-inferior-map))
            (local-set-key (kbd "C-c i r s") 'ruby-send-to-inferior-map)

            (define-key ruby-send-to-inferior-map (kbd "d") 'ruby-send-definition)
            (define-key ruby-send-to-inferior-map (kbd "D") 'ruby-send-definition-and-go)
            (define-key ruby-send-to-inferior-map (kbd "b") 'ruby-send-block)
            (define-key ruby-send-to-inferior-map (kbd "B") 'ruby-send-block-and-go)
            (define-key ruby-send-to-inferior-map (kbd "s") 'ruby-send-region)
            (define-key ruby-send-to-inferior-map (kbd "S") 'ruby-send-region-and-go)
            (define-key ruby-send-to-inferior-map (kbd "R") 'ruby-send-region-and-go)
            ))


;;; [ helm-rb ] -- Search Ruby's method by ag and display helm.


;;; [ helm-robe ]


;;; [ Zossima ] -- Jump to definition in Emacs, driven by a live Ruby subprocess.

;;; Usage:
;;
;; - [M-.]
;; - [M-,]

;; (add-hook 'ruby-mode-hook 'zossima-mode)
;; (add-hook 'enh-ruby-mode-hook 'zossima-mode)


;;; [ ruby-compilation ]

;;; Usage:
;; - [C-x t] -- ruby-compilation-this-buffer
;; - [C-x T] -- ruby-compilation-this-test
;; - [ruby-compilation-run]
;; - [ruby-compilation-rake]
;;
;; - [C-h f ruby-compilation-mode] -- get help.
;; - [C-o] -- compilation-display-error
;; - [RET] / [C-c C-c] -- compile-goto-error
;; - [TAB] / [M-n] -- compilation-next-error
;; - [backtab] / [M-p] -- compilation-previous-error
;; - [g] -- recompile
;; - [C-c C-f] -- next-error-follow-minor-mode
;; - [C-c C-k] -- kill compilation

(eval-after-load "ruby-compilation"
  '(progn
     (define-key enh-ruby-mode-map (kbd "C-c t") 'ruby-compilation-this-buffer)
     (define-key enh-ruby-mode-map (kbd "C-c T") 'ruby-compilation-this-test)
     (define-key ruby-mode-map (kbd "C-c t") 'ruby-compilation-this-buffer)
     (define-key ruby-mode-map (kbd "C-c T") 'ruby-compilation-this-test)
     ))


;;; [ rspec-mode ] -- Ruby RSpec

;;; Usage:
;; - If rspec-mode is installed properly, it will be started automatically when
;;   ruby-mode is started.
;; - [M-x rspec-.*] :: commands

(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

;;; [ Gotchas ]
;;; Debugging
;;
;; To use `binding.pry' or `byebug', install `inf-ruby' and add this to your init file:
(add-hook 'after-init-hook 'inf-ruby-switch-setup)
;; When you've hit the breakpoint, hit [C-x C-q] to enable inf-ruby.

;;; ZSH and RVM
;;
;; If you use ZSH and RVM, you may encounter problems running the specs. It may
;; be so that an older version of Ruby, than the one you specified in .rvmrc, is
;; used. This is because ZSH runs a small script each time a shell is created,
;; which modifies the $PATH. The problem is that it prepends some default paths,
;; such as /usr/bin, which contains another ruby binary.
;;
;; What you can do to solve this is to use BASH for running the specs. This
;; piece of code does the job:

;; (defadvice rspec-compile (around rspec-compile-around)
;;   "Use BASH shell for running the specs because of ZSH issues."
;;   (let ((shell-file-name "/bin/bash"))
;;     ad-do-it))
;;
;; (ad-activate 'rspec-compile)


;;; [ minitest ]

;;; Usage:
;;
;; - [C-c ,] -- minitest prefix
;; `minitest-enable-appropriate-mode'

(setq minitest-default-env nil
      minitest-keymap-prefix (kbd "C-c t") ; [C-c ,]
      minitest-use-bundler t
      minitest-use-spring nil
      minitest-use-zeus-when-possible t
      )

(add-hook 'ruby-mode-hook 'minitest-mode)
(add-hook 'enh-ruby-mode-hook 'minitest-mode)

;; if you want snippets loaded
(eval-after-load 'minitest
  '(minitest-install-snippets))


;;; [ ruby-test-mode ] -- Emacs minor mode for Behaviour and Test Driven Development in Ruby.

;;; Usage:
;;
;; C-c C-,   - Runs the current buffer's file as an unit test or an
;;             rspec example.
;; C-c M-,   - Runs the unit test or rspec example at the current buffer's
;;             buffer's point.
;; C-c C-s   - Toggle between implementation and test/example files.

(dolist (hook '(ruby-mode-hook
                enh-ruby-mode-hook))
  (add-hook hook (lambda ()
                   (unless (boundp 'my-ruby-test-map)
                     (define-prefix-command 'my-ruby-test-map))
                   (local-set-key (kbd "C-c t") 'my-ruby-test-map)

                   (define-key my-ruby-test-map (kbd "m") 'ruby-test-mode)
                   (define-key my-ruby-test-map (kbd "t") 'ruby-test-run)
                   (define-key my-ruby-test-map (kbd "p") 'ruby-test-run-at-point)
                   (define-key my-ruby-test-map (kbd "l") 'ruby-test-goto-location)
                   )))


;;; [ ruby-test ] -- test runner for ruby unit test.

;; This mode provides commands for running ruby tests. The output is shown in
;; separate buffer '*Ruby-Test*' in ruby-test mode. Backtraces from failures and
;; errors are marked, and can be clicked to bring up the relevent source file,
;; where point is moved to the named line.
;;
;; The tests can be both, either rspec behaviours, or unit tests. (File names
;; are assumed to end in `_spec.rb' or `_test.rb' to tell the type.)  When the
;; command for running a test is invoked, it looks at several places for an
;; actual test to run: first, it looks if the current buffer is a test (or
;; spec), secondly, if not, it checks whether one of the visible buffers is,
;; thirdly it looks if there has been a test run before (during this session),
;; in which case that test is invoked again.
;;
;; Using the command `ruby-test-run-test-at-point', you can run test cases
;; separately from others in the same file.

;;; Usage:
;; - [C-x t] -- ruby test: run file.
;; - [C-x SPC] -- ruby test: run file.
;; - [C-x C-SPC] -- ruby test: run test at point.
;; - [C-c t] -- ruby test: toggle implementation and specification.

;; TODO: compare ruby-test & ruby-test-mode.
;; (require 'ruby-test)
;;
;;
;; (dolist (hook '(ruby-mode-hook
;;                 enh-ruby-mode-hook
;;                 ))
;;   (add-hook hook (lambda ()
;;                    (and (local-variable-p 'my-prog-test-map)
;;                       (local-set-key (kbd "C-c t") 'my-prog-test-map))
;;                    (define-key my-prog-test-map (kbd "t") 'ruby-test-run-file)
;;                    (define-key my-prog-test-map (kbd "f") 'ruby-test-run-file)
;;                    (define-key my-prog-test-map (kbd "p") 'ruby-test-run-test-at-point)
;;                    (define-key my-prog-test-map (kbd "SPC") 'ruby-test-run-test-at-point)
;;                    (define-key my-prog-test-map (kbd "i") 'ruby-test-toggle-implementation-and-specification)
;;                    )))


;;; [ ruby-refactor ]

;;; Usage:
;;; Implemented 5 refactorings:
;;
;; - Extract to Method (C-c C-r e)
;; - Extract Local Variable (C-c C-r v)
;; - Extract Constant (C-c C-r c)
;; - Add Parameter (C-c C-r p)
;; - Extract to Let (C-c C-r l)

;; Extract to Method:
;;
;; Select a region of text and invoke ruby-refactor-extract-to-method. You'll be
;; prompted for a method name and a new argument list. If your extracted method
;; does not take parameters, leave it empty. The method will be created above
;; the method you are in with the method contents being the selected region. The
;; region will be replaced with a call to method.
;;
;; Extract Local Variable:
;;
;; Select a region o text and invoke
;; ruby-refactor-extract-local-variable. You'll be prompted for a variable
;; name. The new variable will be created directly above the selected region and
;; the region will be replaced with the variable.
;;
;; Extract Constant:
;;
;; Select a region of text and invoke ruby-refactor-extract-contant. You'll be
;; prompted for a constant name. The new constant will be created at the top of
;; the enclosing class or module directly after any include or extend statements
;; and the regions will be replaced with the constant.
;;
;; Add Parameter:
;;
;; ruby-refactor-add-parameter This simply prompts you for a parameter to add to
;; the current method definition. If you are on a text, you can just hit enter
;; as it will use it by default. There is a custom variable to set if you like
;; parens on your params list. Default values and the like shouldn't confuse it.
;;
;; Extract to Let:
;;
;; This is really for use with RSpec
;;
;; ruby-refactor-extract-to-let There is a variable for where the 'let' gets
;; placed. It can be "top" which is top-most in the file, or "closest" which
;; just walks up to the first describe/context it finds. You can also specify a
;; different regex, so that you can just use "describe" if you want. If you are
;; on a line:
;;
;; a = Something.else.doing
;;
;; becomes
;;
;; let(:a){ Something.else.doing }
;;
;; If you are selecting a region:
;;
;; a = Something.else
;; a.stub(:blah)
;;
;; becomes
;;
;; let :a do
;;   _a = Something.else
;;   _a.stub(:blah)
;;   _a
;; end
;;
;; In both cases, you need the line, first line to have an = in it, as that
;; drives conversion.
;;
;; There is also the bonus that the let will be placed after any other let
;; statements. It appends it to bottom of the list.
;;
;; Oh, if you invoke with a prefix arg (C-u, etc.), it'll swap the placement of
;; the let. If you have location as top, a prefix argument will place it
;; closest. I kinda got nutty with this one.


;; (require 'ruby-refactor)

;; (add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch)



;;; [ yard-mode ] --- Minor mode for Ruby YARD comments

(add-hook 'ruby-mode-hook 'yard-mode)
(add-hook 'enh-ruby-mode-hook 'yard-mode)

;;; If you would also like eldoc support, so that the expected syntax for the
;;; tag beneath your cursor is displayed in the minibuffer, add that hook too:
(add-hook 'ruby-mode-hook 'eldoc-mode)


;;; [ doxymacs-yard ]


;;; [ ruby-lint ]

;;; $ gem install ruby-lint

;; (flycheck-define-checker ruby-rubylint
;;   "A Ruby syntax and style checker using the rubylint tool."
;;   :command ("ruby-lint" source)
;;   :error-patterns
;;   ((warning line-start
;;             (file-name) ":" line ":" column ": " (or "C" "W") ": " (message)
;;             line-end)
;;    (error line-start
;;           (file-name) ":" line ":" column ": " (or "E" "F") ": " (message)
;;           line-end))
;;   :modes (ruby-mode enh-ruby-mode))



;;; [ rubocop ] -- based on Ruby Coding Style Guides

;;; $ gem install rubocop

;; (require 'rubocop)
;; (add-hook 'ruby-mode-hook 'rubocop-mode)

;; (flycheck-define-checker ruby-rubocop
;;   "A Ruby syntax and style checker using the RuboCop tool."
;;   :command ("rubocop" "--format" "emacs" "--silent"
;;             (config-file "--config" flycheck-rubocoprc)
;;             source)
;;   :error-patterns
;;   ((warning line-start
;;             (file-name) ":" line ":" column ": " (or "C" "W") ": " (message)
;;             line-end)
;;    (error line-start
;;           (file-name) ":" line ":" column ": " (or "E" "F") ": " (message)
;;           line-end))
;;   :modes (ruby-mode
;;           ;; enh-ruby-mode motion-mode
;;           ))


;;; [ Rake ]

;;; Usage:
;;
;; `rake' command
;;
;; - [M-x rake] :: to run a rake task.
;; - [C-u M-x rake] :: to amend the command to run. Useful if you want to add arguments.
;; - [C-u C-u M-x rake] :: to bypass the cache (when enabled).
;;
;; `rake-find-task' command
;;
;; - [M-x rake-find-task] :: to find a rake task.
;;
;; Setting up keybinding
;;
;; By default rake command is not bound to any key. You might want to do something like this:
;;
;; (define-key ruby-mode-map (kbd "C-!") 'rake)
;;
;; Replace (kbd "C-!") with a key of your liking.

;;; Caching, By default the caching is enabled. To disable it:
;; (setq rake-enable-caching nil)

;;; Completion, By default ido is used for completion. You can customize it with:
(if (featurep 'projectile)
    (eval-after-load 'projectile
      '(setq rake-completion-system projectile-completion-system))
  (setq rake-completion-system 'helm))


;;; [ motion-mode ] -- RubyMotion

;; (require 'motion-mode)
;;
;; ;; following add-hook is very important.
;; (add-hook 'ruby-mode-hook 'motion-recognize-project)
;; (if (featurep 'auto-complete)
;;     (progn
;;       (add-to-list 'ac-modes 'motion-mode)
;;       (add-to-list 'ac-sources 'ac-source-dictionary)))
;; ;; set key-binds as you like
;; (define-key motion-mode-map (kbd "C-c C-c") 'motion-execute-rake)
;; (define-key motion-mode-map (kbd "C-c C-d") 'motion-dash-at-point)
;;
;; ;; (define-key motion-mode-map (kbd "C-c C-c") 'motion-execute-rake)
;; ;; (define-key motion-mode-map (kbd "C-c C-d") (lambda () (interactive) (motion-execute-rake-command "device")))
;; ;; (define-key motion-mode-map (kbd "C-c C-o") 'motion-dash-at-point)
;; ;; (define-key motion-mode-map (kbd "C-c C-p") 'motion-convert-code-region)


;;; [ Cucumber ]


;;; [ feature-mode ] -- Major mode for Cucumber feature files




(provide 'init-my-prog-lang-ruby)

;;; init-my-prog-lang-ruby.el ends here
