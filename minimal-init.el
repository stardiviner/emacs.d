;;; minimal-init.el --- minimal init file for testing.

;;; Commentary:



;;; Code:

;;; [ Debug ]
(setq debug-on-error t
      debug-on-signal nil
      debug-on-quit nil)

;;; [ profiler ]
;; (profiler-start 'cpu+mem)

;;; benchmark
;; (require 'init-my-emacs-benchmark)



;;; add my init files directory
(let ((default-directory "~/.emacs.d/init/"))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ; shadow
           (append
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

;;; [ package.el ]
(require 'cl)
(require 'package)
(setq package-menu-async t)
(setq package-user-dir "~/.emacs.d/elpa")
(setq-default package-archives
              '(
                ;; ("org"   . "http://orgmode.org/elpa/")
                ("melpa" . "http://melpa.org/packages/")
                ;; ("melpa-stable" . "http://stable.melpa.org/packages/")
                ;; ("marmalade" . "http://marmalade-repo.org/packages/")
                ("gnu" . "https://elpa.gnu.org/packages/")
                ))

(package-initialize)

;;; [ use-package ]
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; [ Quelpa ] -- Build and install your Emacs Lisp packages on-the-fly directly from source.

(use-package quelpa
  :ensure t
  :config
  (setq quelpa-update-melpa-p nil
        ;; quelpa-upgrade-p t
        )

  (add-to-list 'quelpa-melpa-recipe-stores
               (concat user-emacs-directory "melpa/recipes"))
  )

;;; [ Quelpa-use-package ] -- Emacs quelpa handler for use-package.

(use-package quelpa-use-package
  :ensure t)



(require 'color)
(use-package color-theme
  :ensure t)



;;; my custom functions
(require 'init-my-library)
(require 'init-my-functions)

;;; add your customizations from here

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "C-x C-j") 'ace-window)
  )

(use-package which-key
  :ensure
  :config)

(use-package ivy
  :ensure t
  :config)

(use-package company
  :ensure t
  :config
  (setq company-minimum-prefix-length 2
        company-idle-delay 0.2
        company-tooltip-align-annotations t
        company-tooltip-minimum 3
        )

  (setq-default company-backends
                '(company-files         ; files & directory
                  ;; company-gtags company-etags
                  ;; company-tempo         ; tempo: flexible template insertion
                  (company-capf         ; `completion-at-point-functions'
                   :with
                   company-yasnippet)
                  ;; :separate company-semantic
                  :separate company-ispell ; for word completion in comment.
                  (company-keywords
                   :with
                   company-dabbrev-code)
                  company-abbrev
                  )
                )

  (defun my-company-add-backend-locally (backend)
    "Add a backend in my custom way.

\(my-company-add-backend-locally 'company-robe\)
"
    (if (local-variable-if-set-p 'company-backends)
        (add-to-list 'company-backends `(,backend :with company-yasnippet))
      (add-to-list (make-local-variable 'company-backends)
                   `(,backend :with company-yasnippet))
      ))

  (global-company-mode 1)
  
  (define-key company-mode-map (kbd "C-M-i") 'company-complete)

  ;; yasnippet
  ;; `yas-expand', `yas-expand-from-trigger-key'
  (define-key company-active-map [tab] 'yas-expand-from-trigger-key)

  ;; navigation
  (define-key company-active-map (kbd "C-g") 'company-abort)
  (define-key company-active-map (kbd "M-n") 'company-select-next)
  (define-key company-active-map (kbd "M-p") 'company-select-previous)
  (define-key company-active-map (kbd "M-j") 'company-complete-selection)
  (define-key company-active-map (kbd "M-i") 'company-complete-common)

  ;; help
  (define-key company-active-map (kbd "M-h") 'company-show-doc-buffer)

  (define-key company-active-map (kbd "M-l") 'company-show-location)

  ;; search
  (define-key company-active-map (kbd "M-s") 'company-filter-candidates)
  (define-key company-active-map (kbd "C-M-s") 'company-search-candidates)
  (define-key company-search-map (kbd "C-g") 'company-search-abort)
  (define-key company-search-map (kbd "M-s") 'company-search-repeat-forward)
  (define-key company-search-map (kbd "M-r") 'company-search-repeat-backward)
  (define-key company-search-map (kbd "M-n") 'company-search-repeat-forward)
  (define-key company-search-map (kbd "M-p") 'company-search-repeat-backward)
  (define-key company-search-map (kbd "M-o") 'company-search-kill-others)
  (define-key company-search-map (kbd "M-j") 'company-complete-selection)

  (defun company-new-line ()
    "insert a literal return new line."
    (interactive)
    ;; (company-abort)
    (company-cancel 'abort)
    (newline-and-indent)
    )

  (define-key company-active-map [return] 'company-new-line)
  (define-key company-active-map "\r" 'company-new-line)


  ;; [ company-abbrev / company-dabbrev ]
  (setq company-dabbrev-other-buffers t)
  )




;;; latest version
(use-package org
  :load-path "~/Code/Emacs/org-mode/lisp/"
  :pin manual
  :mode (("\\.org$" . org-mode))
  :config
  (use-package org-plus-contrib
    :load-path "~/Code/Emacs/org-mode/contrib/lisp/"
    :pin manual)
  )


;; (require 'init-my-org-mode)

(setq org-modules
      '(org-pcomplete
        org-faces
        ;; org-fstree
        org-table org-compat
        ;; org-protocol
        org-timer org-clock org-habit org-notify
        org-info org-bibtex org-docview
        org-plot
        org-irc ; org-gnus org-mhe org-rmail
        ;; org-w3m
        ))

;;; Org-mode Babel

(setq org-confirm-babel-evaluate nil)
(setq org-babel-no-eval-on-ctrl-c-ctrl-c nil)
(setq org-confirm-shell-link-function 'yes-or-no-p)
(setq org-confirm-elisp-link-function 'yes-or-no-p)

;; babel src block editing
(setq org-src-fontify-natively t
      ;; nil: preserve org indent, t: preserve export indent.
      org-src-preserve-indentation nil
      ;; 0: fix `diff' babel syntax highlighting invalid issue.
      org-edit-src-content-indentation 0
      org-src-tab-acts-natively nil ; make [Tab] work native as in major mode.
      org-src-window-setup 'current-window ; 'reorganize-frame, 'current-window
      org-src-ask-before-returning-to-edit-buffer nil
      org-edit-src-auto-save-idle-delay 0 ; 0: don't auto save.
      )

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)                     ; Emacs Lisp
   (org . t)                            ; Org-mode
   (shell . t)                          ; Shell
   (ruby . t)                           ; Ruby
   (python . t)                         ; Python
   (C . t)                              ; C
   (lisp . t)                           ; Lisp
   (clojure . t)                        ; Clojure
   (js . t)                             ; JavaScript
   (haskell . t)       			; Haskell
   (latex . t)                          ; LaTeX
   ))


;;; [ Clojure ]

;; (require 'init-my-prog-lang-clojure)



;;; [ C/C++ ]

(defvar c-dialects-mode
  '(c-mode
    c++-mode
    objc-mode
    ))

(use-package irony
  :ensure t
  :config
  (hook-modes c-dialects-mode
    (when (memq major-mode irony-supported-major-modes)
      (irony-mode 1)))
  
  ;; load the compile options automatically:
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

  ;; [ company-irony ]
  (use-package company-irony
    :ensure t
    :config
    ;; [ company-irony-c-headers ]
    (use-package company-irony-c-headers
      :ensure t)

    (defun company-irony-add ()
      ;; (optional) adds CC special commands to `company-begin-commands'
      ;; in order to trigger completion at interesting places, such as
      ;; after scope operator.
      ;;     std::|
      (company-irony-setup-begin-commands)

      (make-local-variable 'company-backends)
      (add-to-list 'company-backends
                   '(company-irony
                     :with
                     company-yasnippet))
      (add-to-list 'company-backends 'company-irony-c-headers)
      )

    (hook-modes c-dialects-mode
      (when (memq major-mode irony-supported-major-modes)
        (company-irony-add)))
    )

  ;; [ irony-eldoc ]
  (use-package irony-eldoc
    :ensure t
    :after irony
    :config
    (add-hook 'irony-mode-hook #'irony-eldoc)
    )

  ;; [ flycheck-irony ]
  (use-package flycheck-irony
    :ensure t
    :after irony
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
    )
  )

(use-package yasnippet
  :ensure t
  :config)



(provide 'minimal-init)

;;; minimal-init.el ends here
