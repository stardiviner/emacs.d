;;; init-prog-lang-ruby.el --- my Ruby init

;;; Commentary:


;;; Code:


;; We never want to edit Rubinius bytecode or MacRuby binaries
(add-to-list 'completion-ignored-extensions ".rbc")
(add-to-list 'completion-ignored-extensions ".rbo")


;;; [ ruby-mode ]

(use-package ruby-mode
  :ensure t
  :defer t
  :mode (("\\.irbrc\\'" . ruby-mode))
  :preface (put 'erlang-mode 'derived-mode-parent 'prog-mode)
  ;; :init
  ;; (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
  ;; (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
  :init
  (setq ruby-use-smie t       ; use sexp navigation for Ruby
        ;; bellowing effect only when `ruby-use-smie' is `nil'.
        ruby-deep-indent-paren-style 'space))


;;; [ ob-ruby ]

(use-package ob-ruby
  :defer t
  ;; :custom (org-babel-ruby-command "ruby")
  :commands (org-babel-execute:ruby)
  :config
  (add-to-list 'org-babel-load-languages '(ruby . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("ruby" . "rb")))


;;; [ ruby-tools ] -- Ruby tools is a collection of handy functions for Emacs ruby-mode.

(use-package ruby-tools
  :ensure t
  :defer t
  :config
  (add-hook 'ruby-mode-hook 'ruby-tools-mode)
  (add-hook 'enh-ruby-mode-hook 'ruby-tools-mode))

;;; [ yari ] -- Yet Another Ri Interface

(use-package yari
  :ensure t
  :defer t
  :config
  (defun my-yari-settings ()
    ;; (local-set-key (kbd "C-h d k") 'yari)
    ;; or with document-prefix prefix.
    (unless (boundp 'ruby-help-doc-map)
      (define-prefix-command 'ruby-help-doc-map))
    (local-set-key (kbd "C-h d") 'ruby-help-doc-map)
    (define-key ruby-help-doc-map (kbd "k") 'yari-helm)
    )
  (add-hook 'ruby-mode-hook #'my-yari-settings))

;;; [ rbenv ] -- use rbenv to manage your Ruby versions within Emacs

;; (use-package rbenv
;;   :ensure t
;;   :defer t
;;   :init
;;   (global-rbenv-mode 1)
;;   (rbenv-use-global)
;;   :config
;;   (setq rbenv-show-active-ruby-in-modeline t
;;         rbenv-modeline-function 'rbenv--modeline-plain
;;         )
;;   )


;;; [ inf-ruby ] -- a REPL buffer connected to Ruby(irb/pry) subprocess

(use-package inf-ruby
  :ensure t
  :defer t
  :commands (run-ruby inf-ruby)
  :custom ((inf-ruby-default-implementation "ruby")
           (inf-ruby-prompt-read-only t))
  :hook (ruby-mode . inf-ruby-minor-mode)
  :init (add-to-list 'display-buffer-alist '("^\\*ruby\\*" . (display-buffer-below-selected)))
  :config
  (add-to-list 'inf-ruby-implementations
               '("inf-ruby" . "irb --inf-ruby-mode --noreadline -EUTF-8"))
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
    (define-key enh-ruby-mode-map (kbd "C-c C-s") 'inf-ruby)))


;;; [ Robe ] -- Code navigation, documentation lookup and completion for Ruby.

(use-package robe
  :ensure t
  :defer t
  :init (setq robe-highlight-capf-candidates t
              robe-completing-read-func 'ivy-read)
  (mapc (lambda (hook) (add-hook hook #'robe-mode))
        '(ruby-mode-hook inf-ruby-mode-hook))
  :config (add-hook 'robe-mode-hook
                    (lambda ()
                      (local-set-key (kbd "M-.") 'robe-jump)
                      (local-set-key (kbd "C-h d d") 'robe-doc)))

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
  )

;;; [ lsp-ruby ] -- Ruby support for lsp-mode using the solargraph Gem.

(use-package lsp-mode
  :ensure t
  :defer t
  :hook (ruby-mode . lsp))


(provide 'init-prog-lang-ruby)

;;; init-prog-lang-ruby.el ends here
