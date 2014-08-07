;;; init-my-prog-lang-ruby.el --- my Ruby init

;;; Commentary:


;;; Code:


;;; custom functions

;;; FIXME: key binding always override by flyspell-auto-correct-word.
(defun insert-arrow ()
  "Insert => for Ruby old school style hash."
  (interactive)
  (delete-horizontal-space t)
  (insert " => "))


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

(require 'ruby-mode)

;; (add-auto-mode 'ruby-mode
;;                "Gemfile\\'" "\\.gemspec\\'"
;;                "Rakefile\\'" "\\.rake\\'"
;;                "\\.erb\\'"
;;                "Kirkfile\\'" "Capfile\\'" "Guardfile\\'" "Vagrantfile\\'"
;;                "\\.rxml\\'" "\\.rjs\\'"
;;                "\\.builder\\'" "\\.jbuilder\\'"
;;                "\\.ru\\'" "\\.rabl\\'" "\\.thor\\'"
;;                "\\.irbrc\\'" "\\.pryrc\\'"
;;                )

;; Stupidly the non-bundled ruby-mode isn't a derived mode of
;; prog-mode: we run the latter's hooks anyway in that case.
(add-hook 'ruby-mode-hook
          (lambda ()
            (unless (derived-mode-p 'prog-mode)
              (run-hooks 'prog-mode-hook))

            ;; TODO: modify those colors.
            ;; Words prefixed with $ are global variables,
            ;; prefixed with @ are instance variables.
            (modify-syntax-entry ?$ "w") ; global variable
            (modify-syntax-entry ?@ "w") ; instance variable
            ;; FIXME: (modify-syntax-entry ?@@ "w") ; class variable
            (modify-syntax-entry ?? "w")
            (modify-syntax-entry ?! "w")
            (modify-syntax-entry ?: ".")

            (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
            ;; (define-key ruby-mode-map (kbd "TAB") 'indent-for-tab-command)

            (define-key ruby-mode-map (kbd "C-,") 'insert-arrow)

            ;; hs-minor-mode (hide-show)
            (add-to-list 'hs-special-modes-alist
                         '(ruby-mode
                           "\\(class\\|def\\|do\\|if\\)" "\\(end\\)" "#"
                           (lambda (arg) (ruby-end-of-block)) nil))

            (electric-indent-local-mode 1)
            (setq ruby-use-smie t       ; use sexp navigation for Ruby
                  ;; bellowing effect only when `ruby-use-smie' is `nil'.
                  ruby-deep-indent-paren '(?\( ?\[ ?\] t)
                  ruby-deep-indent-paren-style 'space)
            ))




;;; [ enh-ruby-mode ]

(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

;;; Enhanced Ruby Mode defines its own specific faces with the hook
;;; erm-define-faces. If your theme is already defining those faces, to not
;;; overwrite them, just remove the hook with:
(remove-hook 'enh-ruby-mode-hook 'erm-define-faces)


;;; Others
(add-hook 'enh-ruby-mode-hook
          (lambda ()
            ;; add into auto-complete enable modes.
            (add-to-list 'ac-modes 'enh-ruby-mode)))

(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))


;; We never want to edit Rubinius bytecode or MacRuby binaries
(add-to-list 'completion-ignored-extensions ".rbc")
(add-to-list 'completion-ignored-extensions ".rbo")


;; TODO: whether need this?
;; Font lock for new hash style
;; (font-lock-add-keywords
;;  'ruby-mode
;;  '(("\\(\\b\\sw[_a-zA-Z0-9]*:\\)\\(?:\\s-\\|$\\)" (1 font-lock-constant-face))))



;;; ruby-hash-syntax

;; (require 'ruby-hash-syntax)


;;; [ ruby-block ]

(require 'ruby-block)

(setq ruby-block-delay 0)
(setq ruby-block-highlight-toggle t)
(ruby-block-mode t)


;;; [ ruby-end ]



;;; [ ruby-electric ] -- NOTE: this has been merged into `ruby-mode'.

;; (require 'ruby-electric)

;; (add-hook 'ruby-mode-hook 'ruby-electric-mode)

;; FIXME:
;; (remove-hook 'ruby-mode-hook 'ruby-electric-mode)
;;
;; (add-hook 'ruby-mode-hook
;;           (lambda ()
;;             (require 'ruby-electric)
;;             (autopair-mode -1) ; conflict with ruby-electric.
;;             (ruby-electric-mode t) ; already autoload by el-get?
;;             ))



(add-to-list 'which-func-modes 'ruby-mode)


;;; [ yard-mode ] -- for Ruby YARD comments

(require 'yard-mode)

(add-hook 'ruby-mode-hook 'yard-mode)

;; If you would also like eldoc support, so that the expected syntax for the tag
;; beneath your cursor is displayed in the minibuffer, add that hook too:
(add-hook 'ruby-mode-hook 'eldoc-mode)

;; (add-hook 'enh-ruby-mode-hook 'yard-mode)


;;; [ yari ] -- Yet Another Ri Interface

;; yari.el provides an Emacs frontend to Ruby's `ri' documentation tool. It offers lookup and completion.

(require 'yari)

(dolist (hook '(ruby-mode-hook
                enh-ruby-mode-hook
                ))
  (add-hook hook (lambda ()
                   (setq yari-ri-program-name "ri")

                   ;; (local-set-key (kbd "C-h d") 'yari)
                   ;; (define-key 'help-command (kbd "R") 'yari)
                   
                   ;; or with help-document-map prefix.
                   (unless (boundp 'ruby-help-doc-map)
                     (define-prefix-command 'ruby-help-doc-map))
                   (local-set-key (kbd "C-h d") 'ruby-help-doc-map)
                   (define-key ruby-help-doc-map (kbd "k") 'yari) ; seems minibuffer use ido if ido is enabled.
                   (define-key ruby-help-doc-map (kbd "C-d") 'yari-helm) ; interactive with Helm.
                   )))


;;; [ rvm ] -- integrates Emacs with the rvm (Ruby Version Manager)

;;; Usage:
;;; - [M-x rvm-activate-corresponding-ruby]
;;; - [M-x rvm-use] -- to use another ruby version.
;;; - open the source of any rubygem in your current gemset.
;;;   [M-x rvm-open-gem]

(require 'rvm)
(autoload 'rvm "rvm" "RVM" t)

(rvm-use-default)        ; use rvm's default ruby for the current Emacs session.

(add-hook 'ruby-mode-hook
          (lambda ()
            (rvm-activate-corresponding-ruby)))


;;; [ inf-ruby / Inferior Ruby ] -- inf-ruby provides a REPL buffer connected to a Ruby(irb/pry) subprocess.

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

;; - [M-x run-ruby] -- which runs IRB in an Emacs buffer
;; keybindings:
;; - [C-c C-s] -- inf-ruby
;; - [C-c C-z] -- ruby-switch-to-inf
;; - [C-c C-l] -- ruby-load-file
;; - [M-C-x]   -- ruby-send-definition
;; - [C-x C-e] -- ruby-send-last-sexp
;; - [C-c C-b] -- ruby-send-block
;; - [C-c M-b] -- ruby-send-block-and-go
;; - [C-c C-x] -- ruby-send-definition
;; - [C-c M-x] -- ruby-send-definition-and-go
;; - [C-c C-r] -- ruby-send-region
;; - [C-c M-r] -- ruby-send-region-and-go

;; - [RET] -- after the end of the process' output sends the text from the end of process to point.
;; - [RET] -- before the end of the process' output copies the sexp ending at point
;;           to the end of the process' output, and sends it.
;; - [DEL] -- converts tabs to spaces as it moves back.
;; - [TAB] -- completes the input at point. IRB, Pry and Bond completion is supported.
;;            Helm is supported at here.
;; - [C-M-q] -- does TAB on each line starting within following expression.
;; - Paragraphs are separated only by blank lines. # start comments.
;; - If you accidentally suspend your process, use comint-continue-subjob to continue it.


;; - [C-x C-q] -- rspec / ruby-compilation

;; (require 'inf-ruby)
(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)

(setq inf-ruby-default-implementation "inf-ruby"
      inf-ruby-implementations '(("inf-ruby" . "irb --inf-ruby-mode --noreadline")
                                 ;; ("inf-ruby" . "irb --inf-ruby-mode --prompt inf-ruby")
                                 ("ruby" . "irb --prompt default -r irb/completion --noreadline")
                                 ("jruby" . "jruby -S irb --prompt default -r irb/completion")
                                 ("rubinius" . "rbx -r irb/completion")
                                 ("yarv" . "irb1.9 -r irb/completion")
                                 ("macruby" . "macirb -r irb/completion")
                                 ("pry" . "pry"))
      ;; inf-ruby-orig-compilation-mode nil
      ;; inf-ruby-console-patterns-alist '(("config/application.rb" . rails)
      ;;                                   ("*.gemspec" . gem)
      ;;                                   ("Gemfile" . default))
      inf-ruby-prompt-read-only t
      ;; inf-ruby-eval-binding
      ;; inf-ruby-prompt-format
      ;; inf-ruby-prompt-pattern
      ;; ruby-source-modes '(ruby-mode enh-ruby-mode)
      )

;; integrate with rvm.el
(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))

(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
;; (dolist (mode ruby-source-modes)
;;   (add-hook (intern (format "%s-hook" mode)) 'inf-ruby-minor-mode))

(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(define-key inferior-map (kbd "r a") 'inf-ruby-console-auto)


;;; [ ac-inf-ruby ]

;; (require 'ac-inf-ruby)

;; ;;; for Ruby buffers. {Ruby}
;; (add-hook 'ruby-mode-hook
;;           (lambda ()
;;             (eval-after-load 'auto-complete
;;               (add-to-list 'ac-sources 'ac-source-inf-ruby))
;;             (add-hook 'inf-ruby-minor-mode-hook 'ac-inf-ruby-enable)
;;             ))

;; ;;; for inf-ruby buffer. (Inf-Ruby)
;; (eval-after-load 'auto-complete
;;   (add-to-list 'ac-modes 'inf-ruby-mode))
;; (add-hook 'inf-ruby-mode-hook 'ac-inf-ruby-enable)
;; (eval-after-load 'inf-ruby
;;   (define-key inf-ruby-mode-map (kbd "TAB") 'auto-complete))


;;; [ auto-complete-ruby ]

(require 'auto-complete-ruby)


;;; [ rcodetools ]

(require 'rcodetools)


;;; [ Robe ] -- Code navigation, documentation lookup and completion for Ruby.
;;;
;;; Robe is a code assistance tool that uses a Ruby REPL subprocess with your
;;; application or gem code loaded, to provide information about loaded classes
;;; and modules, and where each method is defined.
;;;
;;; Generally, you'll want to start with `M-x inf-ruby-console-auto'. If there's
;;; no Ruby console running, most interactive commands provided by Robe will
;;; offer to launch it automatically.
;;;
;;; As you change the code in your project, you'll want to update the running
;;; process. To load the current file, type `C-c C-l (ruby-load-file)', see
;;; inf-ruby for more commands. When you're developing a Rails project, you can
;;; type `C-c C-k' instead, to reload the whole environment at once.
;;;
;;; Features:
;;;
;;; Jump to method definition
;;; Jump to super or a constructor called at point
;;; Jump to a module or class (provided it has at least one method defined)
;;; Display method documentation
;;; Display information about method called at point using ElDoc
;;; Method and constant name completion
;;;
;;; To see the available commands, type M-x describe-package RET robe RET.
;;;
;;; Usage:
;; start Robe server.
;; 1. [M-x inf-ruby] :: execute this command in a ruby file buffer.
;; 2. [M-x robe-start]
;;
;; - [C-c C-d] -- robe-doc
;; - [C-c C-k] -- robe-rails-refresh
;; - [M-,] -- pop tag mark
;; - [M-.] -- robe-jump

(require 'robe)
;; (autoload 'robe-mode "robe" "Code navigation, documentation lookup and completion for Ruby" t nil)
;; (autoload 'ac-robe-setup "ac-robe" "auto-complete robe" nil nil)

(add-hook 'ruby-mode-hook 'robe-mode)

(add-hook 'enh-ruby-mode-hook 'robe-mode)

;;; [start robe]
;; The exceptions are code completion and eldoc, which only work if the server
;; is already running. To launch it, type M-x robe-start.
;;; Both of the bellowing work only when the connection to the Ruby subprocess
;;; has been established. To do that, either use one of the core Robe commands,
;;; or type M-x robe-start.
;;
;; FIXME: (error "No matching directory found")
;; (eval-after-load 'robe
;;   (robe-start))

(setq robe-turn-on-eldoc t
      ;; - t, `completion-at-point' candidates buffer will have constants,
      ;; - methods and arguments highlighted in color.
      ;;
      ;; - 'nil, to disable ac-robe face property in ac-menu.
      robe-highlight-capf-candidates t
      )

;; for auto-complete
(add-hook 'robe-mode-hook
          (lambda ()
            (ac-robe-setup)
            ;; (push 'ac-source-robe ac-sources)
            ;; (add-to-list 'ac-sources 'ac-source-robe) ; `ac-robe-setup' did this already.
            ))

;; for company-mode
;; (eval-after-load 'company
;;   (push 'company-robe company-backends))

;; start Robe server.
(inf-ruby)
(robe-start)


(add-hook 'ruby-mode-hook
          (lambda ()
            (local-set-key (kbd "C-h d d") 'robe-doc)
            ;; FIXME: it is not local to ruby-mode only.
            ;; (local-set-key (kbd "C-h d") 'help-document-map)
            ;; (define-key help-document-map (kbd "d") 'robe-doc)
            ))


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

(autoload 'ruby-compilation-this-buffer "ruby-compilation" "run the ruby buffer" t nil)
(autoload 'ruby-compilation-this-test "ruby-compilation" "run the ruby test" t nil)


;;; [ rspec-mode ] -- Ruby RSpec

;;; Usage:
;; - If rspec-mode is installed properly, it will be started automatically when
;;   ruby-mode is started.
;; - [M-x rspec-.*] :: commands

(require 'rspec-mode)

(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

;;; [ Gotchas ]
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
(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))

(ad-activate 'rspec-compile)



;;; [ ruby-refactor ]

;;; Usage:
;;; Implemented 5 refactorings:
;;
;; - Extract to Method (C-c C-r e)
;; - Extract Local Variable (C-c C-r v)
;; - Extract Constant (C-c C-r c)
;; - Add Parameter (C-c C-r p)
;; - Extract to Let (C-c C-r l)

(require 'ruby-refactor)

(add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch)



;;; Usage
;;
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


;;; [ ruby-test ] -- test runner for ruby unit test.

;;; Usage:
;; - [C-x t] -- ruby test: run file.
;; - [C-x SPC] -- ruby test: run file.
;; - [C-x C-SPC] -- ruby test: run test at point.
;; - [C-c t] -- ruby test: toggle implementation and specification.


;;; [ ruby-test-mode ] -- Emacs minor mode for Behaviour and Test Driven Development in Ruby.

;;; Usage:
;;
;; C-c C-,   - Runs the current buffer's file as an unit test or an
;;             rspec example.
;; C-c M-,   - Runs the unit test or rspec example at the current buffer's
;;             buffer's point.
;; C-c C-s   - Toggle between implementation and test/example files.



;;; [ ruby-tools ]


;;; [ rbenv ] -- integrating rbenv with Emacs


;;; [ yard-mode ] --- Minor mode for Ruby YARD comments


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


;;; [ ruby-hash-syntax ] -- automatically convert the selected region of ruby code between 1.8 and 1.9 hash styles.

;;; Usage:
;;
;; Then select a block of ruby code containing a hash literal (perhaps using
;; mark-sexp), and run the `ruby-toggle-hash-syntax' command:

(require 'ruby-hash-syntax)


;;; [ pry (emacs-pry) ] -- Pry support within Emacs

;;; Usage:
;;
;; -`run-pry' :: starts a Pry REPL inside a modified term-mode buffer.
;; - `pry-intercept' :: command allows for quick debuging into a test without
;;   needing to write the binding.pry into the file.
;; - Automatically shows source in emacs buffer.
;; - You can use the pointer or other emacs commands to move the cursor on the
;;   command line and Pry will be aligned with the new position.

(require 'pry)

(define-key my-inferior-ruby-map (kbd "p") 'pry-intercept)
(define-key my-inferior-ruby-map (kbd "C-p") 'pry-intercept-rerun)


;;; [ motion-mode ] -- RubyMotion


;;; [ Cucumber ]



(provide 'init-my-prog-lang-ruby)

;;; init-my-prog-lang-ruby.el ends here
