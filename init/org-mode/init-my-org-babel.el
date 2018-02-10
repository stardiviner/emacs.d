;;; init-my-org-babel.el --- init for Org Babel
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(setq org-confirm-babel-evaluate nil)
(setq org-babel-no-eval-on-ctrl-c-ctrl-c nil)
(setq org-confirm-shell-link-function 'yes-or-no-p)
(setq org-confirm-elisp-link-function 'yes-or-no-p)

(setq org-babel-hash-show-time t) ; header argument: :cache yes.

;;; source block default header arguments
(setq org-babel-default-header-args
      '((:session . "none")
        (:results . "replace") (:cache . "yes")
        (:exports . "code") (:hlines . "no")
        (:noweb . "no") (:tangle . "no")
        (:mkdirp . "yes")
        (:padline . "true") (:comments . "links")
        ))

;; (setq org-babel-default-inline-header-args)
;; (setq org-babel-default-lob-header-args)

;;; Manage org-babel popup buffers with `display-buffer-alist'.
(add-to-list 'display-buffer-alist
             '("^\\*Org- Src.*\\*" (display-buffer-below-selected)))
(add-to-list 'display-buffer-alist
             '("^\\*Org-Babel Preview.*\\*" (display-buffer-below-selected)))
(add-to-list 'display-buffer-alist
             '("^\\*Org-Babel Error Output\\*" (display-buffer-below-selected)))

;;; [ noweb ]
;; Raise errors when noweb references don't resolve.
(setq org-babel-noweb-error-all-langs t)

;;; inline source code header arguments
;; (setq org-babel-default-inline-header-args
;;       '((:session . "none")
;;         (:results . "replace")
;;         (:exports . "both")
;;         (:hlines . "yes")
;;         ))

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

(setq org-babel-load-languages
      '((org . t)                            ; Org-mode
        (shell . t)                          ; Shell Script
        (calc . t)                           ; Calc
        ))

(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)


;;; [ ob-shell ]

(require 'ob-shell)
;; (add-to-list 'org-babel-load-languages '(shell . t))
;; (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
;; (add-to-list 'org-babel-tangle-lang-exts '("shell" . "sh"))

(add-to-list 'org-babel-default-header-args:shell
             '(:results . "output"))
(add-to-list 'org-babel-default-header-args:shell
             '(:noweb . "yes"))


;;; [ ob-uart ] -- A wrapper around make-serial-process for org babel, providing integration of UART communication into org documents.

(use-package ob-uart
  :ensure t
  :defer t
  :config
  (add-to-list 'org-babel-load-languages '(uart . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  ;; FIXME: (add-to-list 'org-babel-tangle-lang-exts '("uart" . "??"))
  )


;;; [ Tangle ]

(setq org-babel-tangle-lang-exts
      '(("clojure" . "clj")
        ("elisp" . "el")
        ("emacs-lisp" . "el")
        ("lisp" . "lisp")
        ("ruby" . "rb")
        ("python" . "py")
        ("R" . "R")
        ("sql" . "sql")
        ("shell" . "sh")
        ("haskell" . "hs")
        ("latex" . "tex")
        ("awk" . "awk")
        ("C" . "c")
        ("Go" . "go")
        ("C++" . "cpp")
        ("perl" . "pl")
        ("js" . "js")
        ("css" . "css")
        ("java" . "java")
        ;; ("rhtml" . "html.erb")
        )
      )

(setq org-babel-tangle-use-relative-file-links t
      ;; org-babel-pre-tangle-hook '(save-buffer)
      ;; org-babel-post-tangle-hook
      )

;;; faster tangleing of large Org mode files.
;; (setq org-babel-use-quick-and-dirty-noweb-expansion t)


;; (defadvice org-babel-execute-src-block (around load-language nil activate)
;;   "Auto load require Babel language libraries `ob-*'."
;;   (let ((language (org-element-property :language (org-element-at-point))))
;;     ;; workaround for #+CALL: babel. (`language' will be `nil')
;;     (if language
;;         ;; whether language is already loaded in `org-babel-load-languages'.
;;         (unless (cdr (assoc (intern language) org-babel-load-languages))
;;           (require (intern (concat "ob-" language)))
;;           (add-to-list 'org-babel-load-languages (cons (intern language) t))
;;           (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
;;       )
;;     ad-do-it))

;; (advice-remove 'org-babel-execute-src-block 'ad-Advice-org-babel-execute-src-block)


;;; language-specific header arguments
;;
;; `org-babel-default-header-args:<lang>' where `<lang>' is the name of the
;; language.  See the language-specific documentation available online at
;; `http://orgmode.org/worg/org-contrib/babel'.


;;; [ Library of Babel ]

;;; automatically ingest "Library of Babel".
(org-babel-lob-ingest (concat user-emacs-directory "Org-mode/Library of Babel/Library of Babel.org"))


;;; interactive completing named src blocks. [C-c C-v C-q]
;; workflow:
;; 1. M-x org-babel-insert-named-source-block (imaginary function)
;; 2. List of named source blocks pops up
;; 3. Hit enter and "#+call: name-of-source-block()" is inserted at point.
(defun org-babel-insert-src-block (&optional template)
  "Interactively insert a named src block."
  (interactive)
  (let ((template (or template "#+call: %s()\n"))
	      (src-block (completing-read "Enter src block name[or TAB or ENTER]: " (org-babel-src-block-names))))
    (unless (string-equal "" src-block)
	    (insert (format template src-block)))))

(define-key org-babel-map (kbd "C-q") 'org-babel-insert-src-block)


;;; [ Literate dotfiles management with Org-mode ]

;; Tangle Org files when we save them
;; (defun tangle-on-save-org-mode-file()
;;   (when (string= (message "%s" major-mode) "org-mode")
;;     (org-babel-tangle)))
;; (add-hook 'after-save-hook 'tangle-on-save-org-mode-file)

;; Enable the auto-revert mode globally. This is quite useful when you have 
;; multiple buffers opened that Org-mode can update after tangling.
;; All the buffers will be updated with what changed on the disk.
;; (global-auto-revert-mode)

;; Add Org files to the agenda when we save them
;; (defun to-agenda-on-save-org-mode-file()
;;   (when (string= (message "%s" major-mode) "org-mode")
;;     (org-agenda-file-to-front)))
;; (add-hook 'after-save-hook 'to-agenda-on-save-org-mode-file)

;; Enable Confluence export
;; (require 'ox-confluence)



;;; change Babel src block background color.
(setq org-src-fontify-natively t)

;; (setq org-src-block-faces
;;       '(("org" (:background (color-darken-name (face-background 'default) 4)))
;;         ("latex" (:background "cyan4"))
;;         ("emacs-lisp" (:background "dark slate gray"))
;;         ("lisp" (:background "DarkGrey"))
;;         ("scheme" (:background "DarkGrey"))
;;         ("clojure" (:background "dark slate blue"))
;;         ("shell" (:background "dark green"))
;;         ("python" (:background "orange1"))
;;         ("ipython" (:background "CadetBlue"))
;;         ("ruby" (:background "HotPink"))
;;         ("perl" (:background "#202020"))
;;         ("php" (:background "SteelBlue4"))
;;         ("C" (:background "SteelBlue4"))
;;         ("C++" (:background "SteelBlue3"))
;;         ("java" (:background "OrangeRed4"))
;;         ("js" (:background "tomato"))
;;         ("javascript" (:background "tomato"))
;;         ("coffee" (:background "dark slate blue"))
;;         ("haskell" (:background "IndianRed"))
;;         ("ocaml" (:background "saddle brown"))
;;         ("sql" (:background "yellow"))
;;         ("sqlite" (:background "yellow"))
;;         ("R" (:background "CadetBlue"))
;;         ("julia" (:background "YellowGreen"))
;;         ("octave" (:background "YellowGreen"))
;;         ("matlab" (:background "YellowGreen"))
;;         ("gnuplot" (:background "YellowGreen"))
;;         ("sclang" (:background "DeepSkyBlue"))
;;         ("ditaa" (:background "violet"))
;;         ("dot" (:background "violet"))
;;         ("plantuml" (:background "violet"))
;;         ("ledger" (:background "LightCoral"))
;;         ("calc" (:background "LightCoral"))
;;         ))



;;; source code block check
(defun org-src-block-check ()
  "Auto check whether src block has language set.
- Report an error if there is a source block without a language specified
- Report an error if there is a source block with a language specified that
is not present in `org-babel-load-languages’
– Check as well for the language of inline code blocks.
– Report the line number instead of the char position.
"
  (interactive)
  (org-element-map (org-element-parse-buffer)
      '(src-block inline-src-block)
    (lambda (sb)
      (let ((language (org-element-property :language sb)))
        (cond ((null language)
               (error "Missing language at line %d in %s"
                      (org-current-line
                       (org-element-property :post-affiliated sb))
                      (buffer-name)))
              ((not (assoc-string language org-babel-load-languages))
               (error "Unknown language `%s' at line %d in `%s'"
                      language
                      (org-current-line
                       (org-element-property :post-affiliated sb))
                      (buffer-name)))))))
  (message "Source blocks checked in %s." (buffer-name (buffer-base-buffer))))

(add-hook 'org-src-mode-hook 'org-src-block-check)

;;; [ coderef ]
;;; prepend comment char ahead of `org-coderef-label'.
;; auto prefix with comment char when create code ref in src block with `org-store-link'.
(use-package smartparens
  :ensure t)
(defun my-org-src-coderef-format (result)
  "Auto prefix with comment char before `org-coderef-label' `RESULT'."
  (if sp-comment-char
      (format "%s %s" sp-comment-char result)
    result))
(advice-add 'org-src-coderef-format :filter-return 'my-org-src-coderef-format)


;;; beacon effect when open org-mode babel src block editing.
(defun my-org-src-edit-animation ()
  (interactive)
  (let ((beacon-size 30)
        (beacon-color "violet red"))
    (beacon-blink)))

(add-hook 'org-src-mode-hook #'my-org-src-edit-animation)


;;; [ ob-async ] -- enables asynchronous execution of org-babel src blocks for *any* languages.

(use-package ob-async
  :ensure t
  ;; :config
  ;; (add-to-list 'org-babel-default-header-args:shell
  ;;              '(:async))
  )

;;; [ org-babel-eval-in-repl ] -- eval org-babel block code with eval-in-repl.el

;; (use-package org-babel-eval-in-repl
;;   :ensure t
;;   :bind (:map org-mode-map
;;               ("C-<return>" . ober-eval-in-repl)
;;               ("C-c C-<return>" . ober-eval-block-in-repl))
;;   :config
;;   (with-eval-after-load "eval-in-repl"
;;     (setq eir-jump-after-eval nil))
;;   )

;; load all languages at last.
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)



(provide 'init-my-org-babel)

;;; init-my-org-babel.el ends here
