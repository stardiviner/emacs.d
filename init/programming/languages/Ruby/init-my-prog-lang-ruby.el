;;; init-my-prog-lang-ruby.el --- my Ruby init

;;; Commentary:


;;; Code:


;; We never want to edit Rubinius bytecode or MacRuby binaries
(add-to-list 'completion-ignored-extensions ".rbc")
(add-to-list 'completion-ignored-extensions ".rbo")


;;; [ ruby-mode ]

(use-package ruby-mode
  :ensure t
  ;; :init
  ;; (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
  ;; (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
  :config
  (setq ruby-use-smie t       ; use sexp navigation for Ruby
        ;; bellowing effect only when `ruby-use-smie' is `nil'.
        ruby-deep-indent-paren-style 'space)
  
  (add-hook 'ruby-mode-hook #'eldoc-mode)
  )


;;; [ enh-ruby-mode ] --

(use-package enh-ruby-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.rb\\'" . enh-ruby-mode))
  (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
  ;; Gemfile, Capfile, Rakefile
  (add-to-list 'auto-mode-alist
               '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'"
                 . enh-ruby-mode))
  ;; irb(irbrc), pry(pryrc), gem(gemspec, gemrc), rackup(ru), Thor(thor),
  (add-to-list 'auto-mode-alist
               '("\\.\\(?:gemspec\\|irbrc\\|pryrc\\|gemrc\\|rake\\|ru\\|thor\\)\\'"
                 . enh-ruby-mode))

  (add-hook 'enh-ruby-mode-hook
            (lambda ()
              (unless (derived-mode-p 'prog-mode)
                (run-hooks 'prog-mode-hook))))

  :config
  (setq enh-ruby-bounce-deep-indent nil
        enh-ruby-deep-arglist t
        enh-ruby-deep-indent-paren t
        enh-ruby-indent-level 2
        enh-ruby-use-encoding-map t
        enh-ruby-use-ruby-mode-show-parens-config nil
        enh-ruby-add-encoding-comment-on-save t
        )
  
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

  (setq enh-ruby-extra-keywords '("self"
                                  "include" "extend"
                                  ;; "private" "protected" "public"
                                  ))

  ;; insert => for hash symbol.
  (defun ruby-mode-insert-symbol-operator ()
    (interactive)
    (insert " => "))
  (define-key enh-ruby-mode-map (kbd "C-;") 'ruby-mode-insert-symbol-operator)
  )


;;; [ ruby-interpolation ] -- Ruby string interpolation helpers.

(use-package ruby-interpolation
  :ensure t)


;;; [ ruby-hash-syntax ] -- automatically convert the selected region of ruby code between 1.8 and 1.9 hash styles.

(use-package ruby-hash-syntax
  :ensure t
  :config
  (with-eval-after-load 'ruby-mode
    (define-key ruby-mode-map (kbd "C-c c c") 'ruby-toggle-hash-syntax))
  (with-eval-after-load 'enh-ruby-mode
    (define-key enh-ruby-mode-map (kbd "C-c c c") 'ruby-toggle-hash-syntax))
  )


;;; [ ruby-tools ] -- Ruby tools is a collection of handy functions for Emacs ruby-mode.

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
;;   :init
;;   (add-hook 'ruby-mode-hook 'ruby-electric-mode)
;;   (add-hook 'enh-ruby-mode-hook 'ruby-electric-mode)
;;   :config
;;   (setq ruby-electric-autoindent-on-closing-char t)
;;   )


;;; [ ruby-end ]

;; (use-package ruby-end
;;   :ensure t
;;   :init
;;   (add-hook 'ruby-mode-hook 'ruby-end-mode)
;;   (add-hook 'enh-ruby-mode-hook 'ruby-end-mode)
;;   )


;;; [ yard-mode ] -- for Ruby YARD comments

(use-package yard-mode
  :ensure t
  :init
  (add-hook 'ruby-mode-hook 'yard-mode)
  (add-hook 'enh-ruby-mode-hook 'yard-mode)
  :config
  ;; workaround of `robe-eldoc'
  ;; (defun yard-eldoc-message ()
  ;;   (cond
  ;;    ((yard-in-comment-p)
  ;;     (let ((tag (yard-tag-at-point)))
  ;;       (when tag (yard-tag-syntax tag))))
  ;;    ((functionp 'robe-eldoc)
  ;;     (robe-eldoc))))
  
  (setq yard-use-eldoc nil)

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

  (defun my-yari-settings ()
    ;; (local-set-key (kbd "C-h d k") 'yari)

    ;; or with my-prog-help-document-map prefix.
    (unless (boundp 'ruby-help-doc-map)
      (define-prefix-command 'ruby-help-doc-map))
    (local-set-key (kbd "C-h d") 'ruby-help-doc-map)
    
    (define-key ruby-help-doc-map (kbd "k") 'yari-helm)
    )

  (dolist (hook '(ruby-mode-hook
                  enh-ruby-mode-hook
                  ))
    (add-hook hook #'my-yari-settings))
  )


;;; [ rvm ] -- integrates Emacs with the rvm (Ruby Version Manager)

;; (use-package rvm
;;   :ensure t
;;   :init
;;   (rvm-use-default)        ; use rvm's default ruby for the current Emacs session.
;;   (dolist (hook '(ruby-mode-hook
;;                   enh-ruby-mode-hook
;;                   ))
;;     (add-hook hook 'rvm-activate-corresponding-ruby))
;;
;;   :config
;;   (setq rvm-verbose t)     ; print rvm switching Ruby version message.
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

  (setq inf-ruby-default-implementation "ruby")
  
  (setq inf-ruby-prompt-read-only t)

  (defun my-inf-ruby-setup ()
    (inf-ruby-minor-mode)

    (add-hook 'completion-at-point-functions
              'inf-ruby-completion-at-point nil t)

    ;; from robe-mode
    (set 'completion-at-point-functions
         (remq 'robe-complete-at-point completion-at-point-functions))
    ;; use `company-robe' instead, because it support doc and meta etc. info
    (setq-local company-minimum-prefix-length 2)
    (my-company-add-backend-locally 'company-robe)
    )
  
  (dolist (hook '(ruby-mode-hook
                  enh-ruby-mode-hook
                  ))
    (add-hook hook 'my-inf-ruby-setup))

  ;; auto type "space" behind inf-ruby buffer line to get rid of company-mode completion.
  (defun my-inf-ruby-return ()
    (interactive)
    (insert " ")
    (comint-send-input))

  (define-key inf-ruby-mode-map (kbd "RET") 'my-inf-ruby-return)
  
  ;; auto switch from common Ruby compilation
  ;; (inf-ruby-setup-auto-breakpoint)
  (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)
  (add-hook 'comint-input-filter-functions 'inf-ruby-auto-exit)

  ;; integrate with rvm.el
  ;; (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  ;;   (rvm-activate-corresponding-ruby))

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

  (define-key enh-ruby-mode-map (kbd "C-c C-s") 'inf-ruby)

  ;; auto start robe process for completing
  (defun my-robe-auto-start ()
    (unless robe-running
      (call-interactively 'inf-ruby)))
  (add-hook 'enh-ruby-mode-hook #'my-robe-auto-start)
  )


;;; [ Robe ] -- Code navigation, documentation lookup and completion for Ruby.

(use-package robe
  :ensure t
  :init
  (dolist (hook '(ruby-mode-hook
                  enh-ruby-mode-hook
                  inf-ruby-mode-hook
                  ))
    (add-hook hook 'robe-mode))
  
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

(use-package rspec-mode
  :ensure t
  :config
  ;; (setq rspec-key-command-prefix (kbd "C-c t r"))
  (setq rspec-key-command-prefix (kbd "C-c ,"))

  ;; run RSpec in Vagrant box.
  (setq rspec-use-vagrant-when-possible t)
  
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

(use-package minitest
  :ensure t
  :config
  (setq minitest-default-env nil
        ;; minitest-keymap-prefix (kbd "C-c t m") ; default [C-c ,]
        minitest-use-bundler t
        minitest-use-spring nil
        minitest-use-zeus-when-possible t
        )

  (add-hook 'ruby-mode-hook 'minitest-mode)
  (add-hook 'enh-ruby-mode-hook 'minitest-mode)

  ;; if you want snippets loaded
  (with-eval-after-load 'minitest
    (minitest-install-snippets))
  )


;;; [ ruby-test-mode ] -- Behaviour and Test Driven Development in Ruby.

(use-package ruby-test-mode
  :ensure t
  :config
  (defun my-ruby-test-mode-settings ()
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
    )

  (dolist (hook '(ruby-mode-hook
                  enh-ruby-mode-hook))
    (add-hook hook #'my-ruby-test-mode-settings))
  )


;;; [ ruby-refactor ]

(use-package ruby-refactor
  :ensure t
  :init
  (add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch)
  )


;;; [ rubocop ] -- based on Ruby Coding Style Guides

(use-package rubocop
  :ensure t
  :init
  (add-hook 'ruby-mode-hook #'rubocop-mode)
  )


;;; [ Rake ]

(use-package rake
  :ensure t
  :config
  ;; 'ivy-read
  (setq rake-completion-system 'default)
  )


;;; [ bundler ] -- Interact with Bundler from Emacs.

(use-package bundler
  :ensure t)


;;; [ motion-mode ] -- RubyMotion

;; (use-package motion-mode
;;   :ensure t
;;   :init
;;   (add-hook 'ruby-mode-hook 'motion-recognize-project)
;;   :config
;;   (when (featurep 'auto-complete)
;;     (add-to-list 'ac-modes 'motion-mode)
;;     (add-to-list 'ac-sources 'ac-source-dictionary))
;;
;;   ;; set key-binds as you like
;;   (define-key motion-mode-map (kbd "C-c C-c") 'motion-execute-rake)
;;   (define-key motion-mode-map (kbd "C-c C-d") 'motion-dash-at-point)
;;
;;   ;; (define-key motion-mode-map (kbd "C-c C-c") 'motion-execute-rake)
;;   ;; (define-key motion-mode-map (kbd "C-c C-d") (lambda () (interactive) (motion-execute-rake-command "device")))
;;   ;; (define-key motion-mode-map (kbd "C-c C-o") 'motion-dash-at-point)
;;   ;; (define-key motion-mode-map (kbd "C-c C-p") 'motion-convert-code-region)
;;   )


;;; [ feature-mode ] -- Major mode for Cucumber feature files

;; (use-package feature-mode
;;   :ensure t)


;;; [ ruby-factory ] -- minor mode for Ruby test object generation libraries.

(use-package ruby-factory
  :ensure t
  :init
  (add-hook 'ruby-mode-hook 'ruby-factory-mode)
  (add-hook 'enh-ruby-mode-hook 'ruby-factory-mode)
  )

;;; [ helm-rdefs ] -- rdefs with helm interface.

(use-package helm-rdefs
  :ensure t
  :bind (:map ruby-mode-map
              ("C-c r" . helm-rdefs)
              :map enh-ruby-mode-map
              ("C-c r" . helm-rdefs))
  )


;;; [ kungfu ] -- CIDER like REPL for Ruby development.

;; (use-package kungfu
;;   :ensure t
;;   :init
;;   (add-hook 'ruby-mode-hook 'ruby-kungfu-mode))

;; (load-file "~/Code/Emacs/ruby-kungfu/kungfu.el")


(provide 'init-my-prog-lang-ruby)

;;; init-my-prog-lang-ruby.el ends here
