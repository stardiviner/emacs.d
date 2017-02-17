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

(use-package org
  :ensure t)

(use-package org-plus-contrib
  :ensure t)

(require 'init-my-org-mode)

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

(setq org-babel-default-header-args
      '((:session . "none")
        (:results . "replace")
        (:exports . "both") (:cache . "no") (:hlines . "no")
        (:noweb . "no") (:tangle . "no")
        (:mkdirp . "yes")
        (:padline . "true") (:comments . "links")
        ))

(setq org-babel-noweb-error-all-langs t)

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

;; latex fragments preview
(setq org-startup-with-latex-preview t)

;; inline image preview
(setq org-startup-with-inline-images nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)                     ; Emacs Lisp
   (org . t)                            ; Org-mode
   (sh . t)                             ; Shell
   (ruby . t)                           ; Ruby
   (python . t)                         ; Python
   (C . t)                              ; C
   (lisp . t)                           ; Lisp
   (scheme . t)                         ; Scheme
   (clojure . t)                        ; Clojure
   (haskell . t)                        ; Haskell
   (js . t)                             ; JavaScript
   (css . t)                            ; CSS
   (latex . t)                          ; LaTeX
   (sql . t)                            ; SQL
   (sqlite . t)                         ; SQLite
   ))

(setq org-todo-keywords
      '(
        ;; Status
        (sequence "URGENT(u!)" "INPROGRESS(g!)" "TODO(t@/!)" "LATER(l!)" "SOMEDAY(s@/!)" "FAILED(x@/!)" "CANCELLED(C@/!)" "|" "DONE(d@/!)")
        ;; Clock
        (sequence "STARTED(!)" "|" "DONE(d@/!)")
        ;; Habit
        (sequence "HABIT(h)" "|" "DONE(d)")
        ;; Types
        (type "CODE(c@/!)" "PROJECT(P@/!)" "Org(o@/!)" "|" "DONE(d@/!)")
        ;; Code
        (sequence "BUG(b!)" "ISSUE(i!)" "ERROR(e!)" "FEATURE(f!)" "Pull-Request(p!)" "|" "DONE(d@/!)")
        ;; Work
        (type "WORK(w@/!)" "MEETING(m@/!)" "|" "DONE(d@/!)")
        ;; Learn
        (type "LEARN(n!)" "REVIEW(r!)" "|" "DONE(d@/!)")
        ;; Life
        (type "SEX(X@/!)" "|" "DONE(d@/!)")
        ;; org-trello
        ;; (type "TODO" "INPROGRESS" "|" "DONE")
        ))

(setq org-todo-keyword-faces
      '(;;; todo keywords
        ("TODO" :foreground "orange"
         :box '(:color "black" :line-width -1))
        ("URGENT" :foreground "red" :background "black"
         :box '(:color "black" :line-width -1))
        ("STARTED" :foreground "green"
         :box '(:color "red" :line-width -1))
        ("HABIT" :foreground "cyan" :background "black"
         :box '(:color "green" :line-width -1))
        ("SOMEDAY" :foreground "dim gray"
         :box '(:color "black" :line-width -1))
        ("INPROGRESS" :foreground "cyan"
         :box '(:color "black" :line-width -1))
        ("LATER" :foreground "dim gray" :background "black"
         :box '(:color "dark red" :line-width -1))
        ("DONE" :foreground "black"
         :strike-through t
         :box '(:color "black" :line-width -1))
        ("FAILED" :foreground "#444444"
         :underline "dark red"
         :box '(:color "black" :line-width -1))
        ("CANCELLED"
         :foreground "black"
         :strike-through t
         :box '(:color "black" :line-width -1))
        ;; code programming
        ("BUG" :foreground "red"
         :box '(:color "red" :line-width -1 :style nil))
        ("ISSUE" :foreground "red"
         :box '(:color "dark red" :line-width -1 :style nil))
        ("ERROR" :foreground "red"
         :box '(:color "red" :line-width -1 :style nil))
        ("FIXME" :foreground "black" :background "red"
         :box '(:color "dark red" :line-width -1 :style nil))
        ("FEATURE" :foreground "cyan"
         :box '(:color "cyan" :line-width -1 :style nil))
        ("Pull-Request" :foreground "yellow"
         :box '(:color "yellow" :line-width -1 :style nil))
        ;; types
        ("Org" :foreground "cyan" :background "#004A5D"
         :box '(:color "cyan" :line-width -1 :style nil))
        ("CODE" :foreground "white" :background "#004A5D"
         :box '(:color "cyan" :line-width -1 :style nil))
        ("PROJECT" :foreground "white" :background "#004A5D"
         :box '(:color "cyan" :line-width -1 :style nil))
        ;; life
        ("SEX" :foreground "deep pink"
         :box '(:color "deep pink" :line-width -1 :style nil))
        ;; work
        ("WORK" :foreground "orange"
         :box '(:color "black" :line-width -1 :style nil))
        ("MEETING" :foreground "cornflower blue"
         :box '(:color "cyan" :line-width -1 :style nil))
        ;; learn
        ("LEARN" :foreground "green yellow"
         :box '(:color "black" :line-width -1))
        ("REVIEW" :foreground "yellow"
         :box '(:color "black" :line-width -1))
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


