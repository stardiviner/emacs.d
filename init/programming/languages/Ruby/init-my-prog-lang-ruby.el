;;; init-my-prog-lang-ruby.el --- my Ruby init

;;; Commentary:


;;; Code:


;; We never want to edit Rubinius bytecode or MacRuby binaries
(add-to-list 'completion-ignored-extensions ".rbc")
(add-to-list 'completion-ignored-extensions ".rbo")


;;; [ ruby-mode ]

(use-package ruby-mode
  :ensure t
  :ensure-system-package ruby
  :mode (("\\.irbrc\\'" . ruby-mode))
  ;; :init
  ;; (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
  ;; (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
  :config
  (setq ruby-use-smie t       ; use sexp navigation for Ruby
        ;; bellowing effect only when `ruby-use-smie' is `nil'.
        ruby-deep-indent-paren-style 'space)
  
  (add-hook 'ruby-mode-hook #'eldoc-mode)
  )


;;; [ ob-ruby ]

(require 'ob-ruby)

(add-to-list 'org-babel-load-languages '(ruby . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(add-to-list 'org-babel-tangle-lang-exts '("ruby" . "rb"))

(add-to-list 'org-babel-default-header-args:ruby
             '(:results . "value"))


;;; [ ruby-tools ] -- Ruby tools is a collection of handy functions for Emacs ruby-mode.

(use-package ruby-tools
  :ensure t
  :init
  (add-hook 'ruby-mode-hook 'ruby-tools-mode)
  (add-hook 'enh-ruby-mode-hook 'ruby-tools-mode)
  )

;;; [ yari ] -- Yet Another Ri Interface

(use-package yari
  :ensure t
  :config
  (defun my-yari-settings ()
    ;; (local-set-key (kbd "C-h d k") 'yari)
    ;; or with prog-doc-map prefix.
    (unless (boundp 'ruby-help-doc-map)
      (define-prefix-command 'ruby-help-doc-map))
    (local-set-key (kbd "C-h d") 'ruby-help-doc-map)
    (define-key ruby-help-doc-map (kbd "k") 'yari-helm)
    )
  (add-hook 'ruby-mode-hook #'my-yari-settings)
  )

;;; [ rbenv ] -- use rbenv to manage your Ruby versions within Emacs

;; (use-package rbenv
;;   :ensure t
;;   :config
;;   (setq rbenv-show-active-ruby-in-modeline t
;;         rbenv-modeline-function 'rbenv--modeline-plain
;;         )
;;   (global-rbenv-mode 1)
;;   (rbenv-use-global)
;;   )


;;; [ inf-ruby ] -- a REPL buffer connected to Ruby(irb/pry) subprocess

(use-package inf-ruby
  :ensure t
  :config
  (add-to-list 'inf-ruby-implementations
               '("inf-ruby" . "irb --inf-ruby-mode --noreadline -EUTF-8"))
  (setq inf-ruby-default-implementation "ruby")
  (setq inf-ruby-prompt-read-only t)

  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode)

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

  (with-eval-after-load 'projectile-rails
    (define-key projectile-rails-mode-map [remap inf-ruby] 'inf-ruby-console-auto))

  ;; ruby-mode has keybinding [C-c C-s] for `inf-ruby'.
  (with-eval-after-load 'ruby-mode
    (define-key ruby-mode-map (kbd "C-c C-s") 'inf-ruby))
  (with-eval-after-load 'enh-ruby-mode
    (define-key enh-ruby-mode-map (kbd "C-c C-s") 'inf-ruby))
  )


;;; [ Robe ] -- Code navigation, documentation lookup and completion for Ruby.

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

  ;; lazily load Ruby source code when saving file.
  (defun my-robe-lazily-load ()
    (interactive)
    (if (or (equal major-mode 'enh-ruby-mode)
            (equal major-mode 'ruby-mode))
        (progn
          (ruby-load-file (buffer-file-name))
          (message "Robe loaded current file code."))))
  ;; (add-hook 'after-save-hook 'my-robe-lazily-load 'append)

  (mapc
   (lambda (hook) (add-hook hook #'robe-mode))
   '(ruby-mode-hook inf-ruby-mode-hook))
  )

;;; [ kungfu ] -- CIDER like REPL for Ruby development.

;; (use-package kungfu
;;   ;; :ensure t
;;   :load-path "~/Code/Emacs/ruby-kungfu/kungfu.el"
;;   :init
;;   (add-hook 'ruby-mode-hook 'ruby-kungfu-mode))


(provide 'init-my-prog-lang-ruby)

;;; init-my-prog-lang-ruby.el ends here
