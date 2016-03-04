;;; init-my-prog-lang-ruby.el --- my Ruby init

;;; Commentary:


;;; Code:


;; We never want to edit Rubinius bytecode or MacRuby binaries
(add-to-list 'completion-ignored-extensions ".rbc")
(add-to-list 'completion-ignored-extensions ".rbo")


;;; [ ruby-mode ]

;; - [C-M-n] -- end of block
;; - [C-M-p] -- beginning of block
;; - [C-M-q] -- (prog-indent-sexp)

;; (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
;; (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

(setq ruby-use-smie t       ; use sexp navigation for Ruby
      ;; bellowing effect only when `ruby-use-smie' is `nil'.
      ruby-deep-indent-paren-style 'space)


;;; [ enh-ruby-mode ] --

(use-package enh-ruby-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.rb\\'" . enh-ruby-mode))
  (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
  ;; Gemfile, Capfile, Rakefile
  (add-to-list 'auto-mode-alist
               '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . enh-ruby-mode))
  ;; irb(irbrc), pry(pryrc), gem(gemspec, gemrc), rackup(ru), Thor(thor),
  (add-to-list 'auto-mode-alist
               '("\\.\\(?:gemspec\\|irbrc\\|pryrc\\|gemrc\\|rake\\|ru\\|thor\\)\\'" . enh-ruby-mode))

  :config
  (setq enh-ruby-bounce-deep-indent nil
        enh-ruby-deep-arglist t
        enh-ruby-deep-indent-paren t
        enh-ruby-indent-level 2
        enh-ruby-use-encoding-map t
        enh-ruby-use-ruby-mode-show-parens-config t
        enh-ruby-add-encoding-comment-on-save t
        )

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
                      :inherit 'flycheck-warning
                      :box nil)
  (set-face-attribute 'erm-syn-errline nil
                      :inherit 'flycheck-error
                      :box nil)


  ;; (electric-indent-local-mode 1)
  ;; TODO: test whether conflict with `ruby-electric'
  ;; (define-key enh-ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
  ;; (define-key enh-ruby-mode-map (kbd "TAB") 'indent-for-tab-command)

  ;; hs-minor-mode (hide-show)
  ;; (add-to-list 'hs-special-modes-alist
  ;;              '(ruby-mode
  ;;                "\\(class\\|def\\|do\\|if\\)" "\\(end\\)" "#"
  ;;                (lambda (arg) (ruby-end-of-block)) nil))


  ;; (setq enh-ruby-extra-keywords '("private" "protected" "public" "self"))
  
  (defun my-ruby-code-custom-highlights ()
    (interactive)
    ;; highlight symbol: dot .
    (font-lock-add-keywords
     'enh-ruby-mode
     '(("[[:alnum:]]\\(\\.\\)[[:alnum:]]"
        (1 '(:foreground "deep pink" :weight 'bold))
        )))
    
    ;; FIXME: this is override by ruby-mode default syntax highlight.
    ;; highlight keyword: self
    ;; (font-lock-add-keywords
    ;;  'enh-ruby-mode
    ;;  '(("\s\\(self\\)\\(\\.\s\\)?"
    ;;     (1 '(:foreground "white" :background "deep pink" :weight 'normal))
    ;;     )))

    ;; highlight keywords: protected(orange), private(dark red), public(white)
    (font-lock-add-keywords
     'enh-ruby-mode
     '(("^\s*\\(public\\)$"
        (1 '(:foreground "white" :weight 'bold :underline "#888888")))))
    (font-lock-add-keywords
     'enh-ruby-mode
     '(("^\s*\\(protected\\)$"
        (1 '(:foreground "yellow" :weight 'bold :underline "#888888")))))
    (font-lock-add-keywords
     'enh-ruby-mode
     '(("^\s*\\(private\\)$"
        (1 '(:foreground "magenta" :weight 'bold :underline "#888888")))))
    )
  
  (add-hook 'enh-ruby-mode-hook 'my-ruby-code-custom-highlights)

  (add-hook 'enh-ruby-mode-hook 'eldoc-mode)
  )


;;; [ ruby-hash-syntax ] -- automatically convert the selected region of ruby code between 1.8 and 1.9 hash styles.

;;; Usage:
;;
;; - region + `ruby-toggle-hash-syntax'

(use-package ruby-hash-syntax
  :ensure t
  :config
  (define-key my-prog-code-map (kbd "c") 'ruby-toggle-hash-syntax) ; convert
  )


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

(use-package ruby-tools
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'ruby-tools-mode)
  (add-hook 'enh-ruby-mode-hook 'ruby-tools-mode)
  )


;;; [ ruby-block ] -- highlight matching block

;; (use-package ruby-block
;;   :ensure t
;;   :config
;;   (setq ruby-block-delay 0)
;;   (setq ruby-block-highlight-toggle t)
;;   (ruby-block-mode t)
;;   )


;;; [ ruby-electric ]

;;; disable this, to fix conflict with other electric functions.
;; (use-package ruby-electric
;;   :ensure t
;;   :config
;;   (setq ruby-electric-autoindent-on-closing-char t)
;;   (add-hook 'ruby-mode-hook 'ruby-electric-mode)
;;   (add-hook 'enh-ruby-mode-hook 'ruby-electric-mode)
;;   )


;;; [ ruby-end ]

(use-package ruby-end
  ;; :ensure t
  ;; :config
  ;; (add-hook 'ruby-mode-hook 'ruby-end-mode)
  ;; (add-hook 'enh-ruby-mode-hook 'ruby-end-mode)
  )


;;; [ yard-mode ] -- for Ruby YARD comments

(use-package yard-mode
  :ensure t
  :config
  (setq yard-use-eldoc nil)
  
  (add-hook 'ruby-mode-hook 'yard-mode)
  (add-hook 'enh-ruby-mode-hook 'yard-mode)

  ;; If you would also like eldoc support, so that the expected syntax for the tag
  ;; beneath your cursor is displayed in the minibuffer, add that hook too:
  ;; (add-hook 'ruby-mode-hook 'eldoc-mode)
  ;; (add-hook 'enh-ruby-mode-hook 'eldoc-mode)
  )


;;; [ yari ] -- Yet Another Ri Interface

(use-package yari
  :ensure t
  :config
  ;; (setq yari-ruby-program-name "ruby"
  ;;       yari-ri-program-name "ri")

  (dolist (hook '(ruby-mode-hook
                  enh-ruby-mode-hook
                  ))
    (add-hook hook
              '(lambda ()
                 ;; (local-set-key (kbd "C-h d k") 'yari)

                 ;; or with my-prog-help-document-map prefix.
                 (unless (boundp 'ruby-help-doc-map)
                   (define-prefix-command 'ruby-help-doc-map))
                 (local-set-key (kbd "C-h d") 'ruby-help-doc-map)
                 
                 (define-key ruby-help-doc-map (kbd "k") 'yari-helm)
                 )))
  )


;;; [ rvm ] -- integrates Emacs with the rvm (Ruby Version Manager)

;;; Usage:
;;; - [M-x rvm-activate-corresponding-ruby]
;;; - [M-x rvm-use] -- to use another ruby version.
;;; - open the source of any rubygem in your current gemset.
;;;   [M-x rvm-open-gem]

;; (use-package rvm
;;   :ensure t
;;   :config
;;   (rvm-use-default)        ; use rvm's default ruby for the current Emacs session.
;;   (setq rvm-verbose t)     ; print rvm switching Ruby version message.
;;
;;   (dolist (hook '(ruby-mode-hook
;;                   enh-ruby-mode-hook
;;                   ))
;;     (add-hook hook 'rvm-activate-corresponding-ruby))
;;   )


;;; [ rbenv ] -- use rbenv to manage your Ruby versions within Emacs

(use-package rbenv
  :ensure t
  :config
  (setq rbenv-show-active-ruby-in-modeline t
        rbenv-modeline-function 'rbenv--modeline-plain
        )

  (global-rbenv-mode 1)

  (rbenv-use-global)
  )


;;; [ inf-ruby ] -- a REPL buffer connected to Ruby(irb/pry) subprocess

(use-package inf-ruby
  :ensure t
  :config
  (add-to-list 'inf-ruby-implementations
               '("inf-ruby" . "irb --inf-ruby-mode --noreadline -EUTF-8"))
  
  (setq inf-ruby-default-implementation "ruby"
        inf-ruby-prompt-read-only t
        )

  ;; integrate with rvm.el
  ;; (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  ;;   (rvm-activate-corresponding-ruby))

  (dolist (hook '(ruby-mode-hook
                  enh-ruby-mode-hook
                  ))
    (add-hook hook
              (lambda ()
                (inf-ruby-minor-mode)
                
                (make-local-variable 'completion-at-point-functions)

                ;; from inf-ruby
                (add-to-list 'completion-at-point-functions 'inf-ruby-completion-at-point)

                ;; from robe-mode
                ;; (remq 'robe-complete-at-point completion-at-point-functions)
                ;; (append '(robe-complete-at-point) completion-at-point-functions)
                ;; use `company-robe' instead, because it support doc and meta etc. info
                (my-company-add-backends-to-mode '(company-robe))
                )))

  ;; ruby-mode has keybinding [C-c C-s] for `inf-ruby'.
  ;; auto start robe `robe-start' after start `inf-ruby'.
  (defun my-robe-start ()
    (interactive)
    (unless robe-running
      (robe-start)))

  (defadvice inf-ruby-console-auto (after inf-ruby-console-auto activate)
    "Run `robe-start' after `inf-ruby-console-auto' started."
    (my-robe-start))

  (with-eval-after-load 'projectile-rails
    (define-key projectile-rails-mode-map
      [remap inf-ruby] 'inf-ruby-console-auto))

  (defadvice inf-ruby (after inf-ruby activate)
    "Run `robe-start' after `inf-ruby' started."
    (my-robe-start))

  ;; (define-key enh-ruby-mode-map (kbd "C-c C-s") 'inf-ruby)
  ;; (define-key my-prog-inferior-map (kbd "r a") 'inf-ruby-console-auto)

  ;; auto switch from common Ruby compilation
  ;; (inf-ruby-setup-auto-breakpoint)
  (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)
  (add-hook 'comint-input-filter-functions 'inf-ruby-auto-exit)
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

;; (use-package pry
;;   :ensure t
;;   :config
;;   ;; (setq pry-program-name "pry")           ; program invoked by the `run-pry' command.
;;
;;   (define-key my-inferior-ruby-map (kbd "p") 'run-pry)
;;   (define-key my-inferior-ruby-map (kbd "C-p") 'pry-intercept)
;;   (define-key my-inferior-ruby-map (kbd "C-r") 'pry-intercept-rerun)
;;   )


;;; [ Robe ] -- Code navigation, documentation lookup and completion for Ruby.

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

(use-package robe
  :ensure t
  :config

  (setq robe-highlight-capf-candidates t
        robe-completing-read-func 'ivy-read)

  (add-hook 'robe-mode-hook
            (lambda ()
              (local-set-key (kbd "M-.") 'robe-jump)
              (local-set-key (kbd "C-h d d") 'robe-doc)
              ))

  (with-eval-after-load 'projectile-rails
    (define-key projectile-rails-mode-map (kbd "C-h d d") 'robe-doc))

  (dolist (hook '(ruby-mode-hook
                  enh-ruby-mode-hook
                  inf-ruby-mode-hook
                  ))
    (add-hook hook 'robe-mode))

  ;; lazily load Ruby source code when saving file.
  (defun my-robe-lazily-load ()
    (interactive)
    (if (or (equal major-mode 'enh-ruby-mode)
            (equal major-mode 'ruby-mode))
        (progn
          (ruby-load-file (buffer-file-name))
          (message "Robe loaded current file code."))
      ))
  ;; (add-hook 'after-save-hook 'my-robe-lazily-load 'append)
  )


;;; [ rspec-mode ] -- Ruby RSpec

;;; Usage:
;;
;; - [C-c ,] :: keybindings prefix.

(use-package rspec-mode
  :ensure t
  :init
  ;; (setq rspec-key-command-prefix (kbd "C-c t r"))
  (setq rspec-key-command-prefix (kbd "C-c ,"))
  :config
  ;; (dolist (hook '(ruby-mode-hook
  ;;                 enh-ruby-mode-hook
  ;;                 ))
  ;;   (add-hook hook 'rspec-mode))

  ;; my custom meaningful rspec-mode keybindings

  (defun rspec-open-spec-other-buffer ()
    (interactive)
    (when (featurep 'rspec-mode)
      (let ((source-buffer (current-buffer))
            (other-buffer (progn
                            (rspec-toggle-spec-and-target)
                            (current-buffer))))
        (switch-to-buffer source-buffer)
        (pop-to-buffer other-buffer))))

  (defun my-rspec-mode-custom-keybindings ()
    (interactive)
    
    (unless (boundp 'my-rspec-mode-map)
      (define-prefix-command 'my-rspec-mode-map))
    (unless (boundp 'rspec-find-map)
      (define-prefix-command 'rspec-find-map))
    (unless (boundp 'rspec-verify-map)
      (define-prefix-command 'rspec-verify-map))
    (unless (boundp 'rspec-toggle-map)
      (define-prefix-command 'rspec-toggle-map))
    
    (local-set-key (kbd "C-c ,") 'my-rspec-mode-map)
    (define-key my-rspec-mode-map (kbd "v") 'rspec-verify-map)
    (define-key my-rspec-mode-map (kbd "t") 'rspec-toggle-map)

    (define-key my-rspec-mode-map (kbd ",") 'rspec-open-spec-other-buffer)
    
    ;; find
    (define-key my-rspec-mode-map (kbd "f") 'rspec-find-spec-or-target-other-window)
    (define-key my-rspec-mode-map (kbd "F") 'rspec-find-spec-or-target-find-example-other-window)
    ;; verify
    (define-key rspec-verify-map (kbd "v") 'rspec-verify)
    (define-key rspec-verify-map (kbd "c") 'rspec-verify-continue)
    (define-key rspec-verify-map (kbd "a") 'rspec-verify-all)
    (define-key rspec-verify-map (kbd "M") 'rspec-verify-matching)
    (define-key rspec-verify-map (kbd "m") 'rspec-verify-method)
    (define-key rspec-verify-map (kbd "s") 'rspec-verify-single)
    ;; run
    (define-key my-rspec-mode-map (kbd "r") 'rspec-run-last-failed)
    (define-key my-rspec-mode-map (kbd "R") 'rspec-rerun)
    ;; toggle
    (define-key rspec-toggle-map (kbd "t") 'rspec-toggle-spec-and-target)
    (define-key rspec-toggle-map (kbd "e") 'rspec-toggle-spec-and-target-find-example)
    (define-key rspec-toggle-map (kbd "p") 'rspec-toggle-example-pendingness)
    )
  
  (add-hook 'rspec-mode-hook 'my-rspec-mode-custom-keybindings)
  
  ;; [ Gotchas ]
  ;; Debugging
  ;;
  ;; To use `binding.pry' or `byebug', install `inf-ruby' and add this to your
  ;; init file:
  (add-hook 'after-init-hook 'inf-ruby-switch-setup)
  ;; When you've hit the breakpoint, hit [C-x C-q] to enable inf-ruby.

  ;; ZSH and RVM
  ;;
  ;; If you use ZSH and RVM, you may encounter problems running the specs. It may
  ;; be so that an older version of Ruby, than the one you specified in .rvmrc, is
  ;; used. This is because ZSH runs a small script each time a shell is created,
  ;; which modifies the $PATH. The problem is that it prepends some default paths,
  ;; such as /usr/bin, which contains another ruby binary.
  ;;
  ;; What you can do to solve this is to use BASH for running the specs. This
  ;; piece of code does the job:
  ;;
  ;; (defadvice rspec-compile (around rspec-compile-around)
  ;;   "Use BASH shell for running the specs because of ZSH issues."
  ;;   (let ((shell-file-name "/bin/bash"))
  ;;     ad-do-it))
  ;;
  ;; (ad-activate 'rspec-compile)

  ;; (eval-after-load 'rspec-mode
  ;;   '(rspec-install-snippets))
  )


;;; [ minitest ]

;;; Usage:
;;
;; - [C-c ,] -- minitest keybindings prefix.
;; `minitest-enable-appropriate-mode'

(use-package minitest
  :ensure t
  :config
  (setq minitest-default-env nil
        ;; minitest-keymap-prefix (kbd "C-c t m") ; default [C-c ,]
        minitest-use-bundler t
        minitest-use-spring nil
        minitest-use-zeus-when-possible t
        )

  ;; (add-hook 'ruby-mode-hook 'minitest-mode)
  ;; (add-hook 'enh-ruby-mode-hook 'minitest-mode)

  ;; if you want snippets loaded
  (eval-after-load 'minitest
    '(minitest-install-snippets))
  )


;;; [ ruby-test-mode ] -- Behaviour and Test Driven Development in Ruby.

(use-package ruby-test-mode
  :ensure t
  :config
  (dolist (hook '(ruby-mode-hook
                  enh-ruby-mode-hook))
    (add-hook hook
              '(lambda ()
                 ;; remove default ruby-test-mode in ruby-mode-hook.
                 ;; [C-c C-s] conflict with inf-ruby.
                 (remove-hook 'ruby-mode-hook 'ruby-test-enable)
                 
                 (unless (boundp 'my-ruby-test-map)
                   (define-prefix-command 'my-ruby-test-map))
                 (local-set-key (kbd "C-c t") 'my-ruby-test-map)

                 (define-key my-ruby-test-map (kbd "m") 'ruby-test-mode)
                 (define-key my-ruby-test-map (kbd "t") 'ruby-test-run)
                 (define-key my-ruby-test-map (kbd "p") 'ruby-test-run-at-point)
                 (define-key my-ruby-test-map (kbd "l") 'ruby-test-goto-location)
                 )))
  )


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


(use-package ruby-refactor
  ;; :ensure t
  :config
  (add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch)
  )


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

(use-package rake
  :ensure t
  :config
  ;; 'ivy-read
  (setq rake-completion-system 'default)
  )


;;; [ bundler ] -- Interact with Bundler from Emacs.

;;; Usage:
;;
;; - `bundle-open' :: wraps bundle open which, if the given gem is installed and
;;   has been required correctly, will open the gem's source directory with
;;   dired.
;;
;; - `bundle-console' :: starts an inferior ruby process in the context of the
;;   current bundle using 'bundle console' (requires inf-ruby to be installed).
;;
;; - `bundle-install', `bundle-update', `bundle-check' :: run the corresponding
;;   Bundler commands with `async-shell-command' and *Bundler* as the target
;;   buffer. This exists so the output won't mess with the default buffer used
;;   by [M-&] and `async-shell-command'.

(use-package bundler
  :ensure t)


;;; [ motion-mode ] -- RubyMotion

(use-package motion-mode
  :ensure t
  :config
  ;; following add-hook is very important.
  (add-hook 'ruby-mode-hook 'motion-recognize-project)
  (if (featurep 'auto-complete)
      (progn
        (add-to-list 'ac-modes 'motion-mode)
        (add-to-list 'ac-sources 'ac-source-dictionary)))
  ;; set key-binds as you like
  (define-key motion-mode-map (kbd "C-c C-c") 'motion-execute-rake)
  (define-key motion-mode-map (kbd "C-c C-d") 'motion-dash-at-point)

  ;; (define-key motion-mode-map (kbd "C-c C-c") 'motion-execute-rake)
  ;; (define-key motion-mode-map (kbd "C-c C-d") (lambda () (interactive) (motion-execute-rake-command "device")))
  ;; (define-key motion-mode-map (kbd "C-c C-o") 'motion-dash-at-point)
  ;; (define-key motion-mode-map (kbd "C-c C-p") 'motion-convert-code-region)
  )


;;; [ Cucumber ]


;;; [ feature-mode ] -- Major mode for Cucumber feature files

(use-package feature-mode
  :ensure t)


;;; [ ruby-factory ] -- minor mode for Ruby test object generation libraries.

;;; Usage:
;;
;; - [C-c , j] :: Jump to the current buffer's model or factory
;; - Snippets

;;; Supports:
;; - factory_girl
;; - Fabrication
;; - only under Rails (for now).

(use-package ruby-factory
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'ruby-factory-mode)
  (add-hook 'enh-ruby-mode-hook 'ruby-factory-mode)
  )


(provide 'init-my-prog-lang-ruby)

;;; init-my-prog-lang-ruby.el ends here
