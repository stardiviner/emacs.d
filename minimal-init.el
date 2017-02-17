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
              '(("org"   . "http://orgmode.org/elpa/")
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
;; (require 'init-my-library)
;; (require 'init-my-functions)

;;; add your customizations from here

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "C-x C-j") 'ace-window)
  )

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
                  (company-capf         ; `completion-at-point-functions'
                   :with
                   company-yasnippet)
                  company-dabbrev-code  ; company-dabbrev
                  company-abbrev
                  company-keywords      ; keywords
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


