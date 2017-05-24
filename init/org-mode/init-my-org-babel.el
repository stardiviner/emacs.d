;;; init-my-org-babel.el --- init for Org Babel
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(setq org-confirm-babel-evaluate nil)
(setq org-babel-no-eval-on-ctrl-c-ctrl-c nil)
(setq org-confirm-shell-link-function 'yes-or-no-p)
(setq org-confirm-elisp-link-function 'yes-or-no-p)

;;; source block header arguments
(setq org-babel-default-header-args
      '((:session . "none")
        (:results . "replace")
        (:exports . "both") (:cache . "no") (:hlines . "no")
        (:noweb . "no") (:tangle . "no")
        (:mkdirp . "yes")
        (:padline . "true") (:comments . "links")
        ))

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

;;; babel eval result
(setq org-babel-inline-result-wrap "=%s="
      org-export-babel-evaluate 'inline-only
      )

;;; [ ob-emacs-lisp ]

(add-to-list 'org-babel-default-header-args:emacs-lisp
             '(:results . "output"))

;;; [ ob-lisp ]
;; (setq org-babel-lisp-eval-fn #'sly-eval)

;;; [ ob-clojure ]

(require 'ob-clojure)
(add-to-list 'org-babel-default-header-args:clojure
             '(:results . "output"))
;; (add-to-list 'org-babel-default-header-args:clojure
;;              '(:show-process . t))

;;; [ ob-haskell ]

(require 'ob-haskell)

(add-to-list 'org-babel-default-header-args:haskell
             '(:session . "*haskell*"))

;;; [ ob-latex ]
(require 'ob-latex)

;; (require 'ob-js)
;; (setq org-babel-js-cmd "node")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)                     ; Emacs Lisp
   (org . t)                            ; Org-mode
   (sh . t)                             ; Shell
   (shell . t)                          ; Shell Script
   (makefile . t)                       ; Make
   (python . t)                         ; Python
   (ruby . t)                           ; Ruby
   ;; (perl . t)                           ; Perl
   (C . t)                              ; C
   (java . t)                           ; Java
   ;; (lua . t)                            ; Lua
   ;; (awk . t)                            ; Awk
   ;; (sed . t)                            ; Sed
   ;; (screen . t)                         ; Screen
   (lisp . t)                           ; Lisp
   (scheme . t)                         ; Scheme
   ;; (picolisp . t)                       ; Pico Lisp
   (clojure . t)                        ; Clojure
   (haskell . t)                        ; Haskell
   ;; (scala . t)                          ; Scala
   ;; (io . t)                             ; IO
   ;; (J . t)                              ; J
   ;; (ocaml . t)                          ; Objective Caml
   (js . t)                             ; JavaScript
   (css . t)                            ; CSS
   (latex . t)                          ; LaTeX
   (sql . t)                            ; SQL
   (sqlite . t)                         ; SQLite
   ;; (matlab . t)                         ; MATLAB
   ;; (octave . t)                         ; Octave
   ;; (gnuplot . t)                        ; gnuplot
   ;; (fortran . t)                        ; Fortran
   (ditaa . t)                          ; ditaa
   (dot . t)                            ; Graphviz, Dot
   (plantuml . t)                       ; PlantUML
   ;; (ebnf2ps . t)                        ; ebnf2ps
   (calc . t)                           ; Calc
   (ledger . t)                         ; ledger support in Babel
   ;; (asymptote . t)                      ; Asymptote
   ;; (sass . t)                           ; Sass
   ;; (processing . t)                     ; Processing
   ;; -- Extra --
   ;; use advice: `org-babel-execute-src-block' to load language support lazily.
   ;; (C++ . t)                            ; C++
   ;; (D . t)                              ; D
   (R . t)                              ; R
   ;; (go . t)
   (ipython . t)
   ;; (sagemath . t)        ; `ob-sagemath'
   ;; (restclient . t)                     ; `ob-restclient'
   (elasticsearch . t)                  ; `es-mode'
   (mongo . t)                          ; `ob-mongo'
   (uart . t)                           ; `ob-uart'
   ))

;;; [ Tangle ]
(setq org-babel-tangle-lang-exts
      '(("latex" . "tex")
        ("elisp" . "el")
        ("emacs-lisp" . "el")
        ("lisp" . "lisp")
        ("ruby" . "rb")
        ("python" . "py")
        ("R" . "R")
        ("sql" . "sql")
        ("sh" . "sh")
        ("haskell" . "hs")
        ("clojure" . "clj")
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



(defadvice org-babel-execute-src-block (around load-language nil activate)
  "Load language if needed."
  (let ((language (org-element-property :language (org-element-at-point))))
    ;; workaround for #+CALL: babel. (`language' will be `nil')
    (if language
        ;; whether language is already loaded in `org-babel-load-languages'.
        (unless (cdr (assoc (intern language) org-babel-load-languages))
          (require (intern (concat "ob-" language)))
          (add-to-list 'org-babel-load-languages (cons (intern language) t))
          (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
      )
    ad-do-it))


;; NOTE: this may cause org babel block syntax highlighting failed.

(add-to-list 'org-src-lang-modes (cons "SQL" 'sql))

(with-eval-after-load 'js2-mode
  (add-to-list 'org-src-lang-modes '("js" . js2)))

(add-to-list 'org-src-lang-modes '("ruby" . enh-ruby))

(with-eval-after-load 'web-mode
  (add-to-list 'org-src-lang-modes '("html" . web))
  (add-to-list 'org-src-lang-modes '("rhtml" . web))
  )


;;;_ + ditaa & PlantUML & Graphviz

;; Org-babel makes it easy to generate decent graphics using external packages
;; like ditaa, graphviz, PlantUML, and others.
;;
;; The setup is really easy. ditaa is provided with the org-mode source. You'll
;; have to install the `graphviz' and `PlantUML' packages on your system.

;; ditaa & PlantUML setup
(setq org-ditaa-jar-path "~/.emacs.d/init/extra/ditaa0_9.jar")
(setq org-plantuml-jar-path "~/.emacs.d/init/extra/plantuml.jar")

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images 'append)

;;; PlantUML language reference
;;
;; [[file:~/.emacs.d/init/extra/PlantUML%20Language%20Reference%20Guide.pdf][PlantUML Language Reference Guide]]

;; Use fundamental mode when editing plantuml blocks with C-c '
;; (add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))

;;; Graphviz

;;; Example
;; #+BEGIN_SRC dot :file some_filename.png :cmdline -Kdot -Tpng
;;   <context of graphviz source goes here>
;; #+END_SRC



;;; language-specific header arguments

;; `org-babel-default-header-args:<lang>' where `<lang>' is the name of the
;; language.  See the language-specific documentation available online at
;; `http://orgmode.org/worg/org-contrib/babel'.

;; generate results as #+BEGIN_LaTeX ... #+END_LaTeX block.
;; (setq org-babel-default-header-args:latex
;;       '((:results . "latex")
;;         (:exports . "results")
;;         ))

;; let latex babel generate image result
;; (setq org-babel-default-header-args:latex
;;       '((:results . "raw graphics")
;;         (:file . "temp.png")))

(add-to-list 'org-babel-default-header-args:sh
             '(:results . "output"))

(add-to-list 'org-babel-default-header-args:ruby
             '(:results . "output"))

(add-to-list 'org-babel-default-header-args:C
             '(:results . "output"))

;; (add-to-list 'org-babel-default-header-args:C++
;;              '(:results . "output"))

(add-to-list 'org-babel-default-header-args:js
             '(:results . "output"))

(setq org-babel-default-header-args:R
      '((:session . "*R*")
        (:exports . "both")
        (:results . "replace")
        ))

;; (add-to-list 'org-babel-default-header-args:ocaml
;;              '(:results . "value"))

(setq org-babel-default-header-args:sqlite
      '((:db . "temp.db")
        (:results . "raw")
        ;; (:echo . t)
        (:column . t)
        (:nullvalue . "Null")))

;;; [ ob-ledger ]

;; (setq org-babel-default-header-args:ledger
;;       '((:results . "output") (:cmdline . "bal"))
;;       )

(with-eval-after-load 'org-capture
  (setq org-capture-templates
      (append '(("l" "[L]edger entries")
                ;; Expense
                ("le" "[E]xpenses")
                ("les" "[S]hopping" plain
                 (file (concat org-directory "/Accounting/my.ledger"))
                 "%(org-read-date) %^{Event}\n expenses:shopping:%^{category}  %^{Amount}")
                ("lef" "[F]ood" plain
                 (file (concat org-directory "/Accounting/my.ledger"))
                 "%(org-read-date) %^{Event}\n expenses:food:%^{meat,breakfast,lunch,dinner}  %^{Amount}")
                ("let" "[T]raffic" plain
                 (file (concat org-directory "/Accounting/my.ledger"))
                 "%(org-read-date) %^{Event}\n expenses:traffic:%^{bus,train,plane}  %^{Amount}")
                ("leh" "[H]ouse Rent" plain
                 (file (concat org-directory "/Accounting/my.ledger"))
                 "%(org-read-date) %^{Event}\n expenses:house rent:  %^{Amount}")
                
                ;; Income
                ("li" "[I]ncome")
                ("lis" "[S]alary" plain
                 (file (concat org-directory "/Accounting/my.ledger"))
                 "%(org-read-date) %^{Event}\n income:salary:%^{account}  %^{Amount}")

                ;; Transfer
                ("lt" "[T]ransfer")
                ("ltb" "Take out money from [B]ank"
                 (file (concat org-directory "/Accounting/my.ledger"))
                 "%(org-read-date) %^{Event}\n transfer:%^{source} -> %^{bank}  %^{Amount}"
                 )
                ("lto" "save moeny on [o]nline account"
                 (file (concat org-directory "/Accounting/my.ledger"))
                 "%(org-read-date) %^{Event}\n transfer:%^{source} -> %^{ZhiFuBao}  %^{Amount}"
                 )
                ("ltc" "take out moeny to [C]ash"
                 (file (concat org-directory "/Accounting/my.ledger"))
                 "%(org-read-date) %^{Event}\n transfer:%^{source} -> cash  %^{Amount}"
                 )

                ;; Debt
                ("ld" "[D]ebt")
                ("ldr" "[R]ent" plain
                 (file (concat org-directory "/Accounting/my.ledger"))
                 "%(org-read-date) %^{Event}\n debt:rent:%^{people}  %^{Amount}")
                ("ldb" "[B]orrow" plain
                 (file (concat org-directory "/Accounting/my.ledger"))
                 "%(org-read-date) %^{Event}\n debt:borrow:%^{people}  %^{Amount}")
                
                ;; Assets
                ("la" "[A]ssets")
                ("lab" "[B]ank" plain
                 (file (concat org-directory "/Accounting/my.ledger"))
                 "%(org-read-date) %^{Event}\n assets:bank:%^{bank}  %^{Amount}")
                ("lao" "[O]nline Accounts" plain
                 (file (concat org-directory "/Accounting/my.ledger"))
                 "%(org-read-date) %^{Event}\n assets:online-account:%^{ZhiFuBao}  %^{Amount}")
                )
              org-capture-templates))
  )



;;; [ Library of Babel ]

(org-babel-lob-ingest (concat user-emacs-directory "Org-mode/Library of Babel/Library of Babel.org"))

;;;_ * source code block check
;;
;; - Report an error if there is a source block without a language specified
;; - Report an error if there is a source block with a language specified that
;;   is not present in `org-babel-load-languages’
;; – “Check as well for the language of inline code blocks,”
;; – “Report the line number instead of the char position.”

(defun org-src-block-check ()
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


;; how to correctly enable flycheck in babel source blocks
(defadvice org-edit-src-code (around set-buffer-file-name activate compile)
  (let ((file-name (buffer-file-name)))
    ad-do-it
    (setq buffer-file-name file-name)))


;;; auto format (indent) source code block.

;; (defun indent-org-block-automatically ()
;;   (when (org-in-src-block-p)
;;     (org-edit-special)
;;     (indent-region (point-min) (point-max))
;;     (org-edit-src-exit)))
;;
;; (run-at-time 1 10 'indent-org-block-automatically)


;;; Templates -- (org skeleton/template)


;;; [ ob-translate ] -- allows you to translate blocks of text within org-mode.

(use-package ob-translate
  :ensure t)


;;; [ Literate Programming with Org-mode ]

;;; [ Clojure ]

;; use CIDER as the Clojure execution backend
(setq org-babel-clojure-backend 'cider)

;; Useful keybindings when using Clojure from Org
;; (org-defkey org-mode-map (kbd "C-x C-e") 'cider-eval-last-sexp)
;; (org-defkey org-mode-map (kbd "C-c C-d") 'cider-doc)

;; No timeout when executing calls on Cider via nrepl
;; (setq org-babel-clojure-sync-nrepl-timeout nil)

;; (setq org-support-shift-select 'always)

;; Tangle Org files when we save them
;; (defun tangle-on-save-org-mode-file()
;;   (when (string= (message "%s" major-mode) "org-mode")
;;     (org-babel-tangle)))
;; (add-hook 'after-save-hook 'tangle-on-save-org-mode-file)

;; Enable the auto-revert mode globally. This is quite useful when you have 
;; multiple buffers opened that Org-mode can update after tangling.
;; All the buffers will be updated with what changed on the disk.
(global-auto-revert-mode)

;; Add Org files to the agenda when we save them
;; (defun to-agenda-on-save-org-mode-file()
;;   (when (string= (message "%s" major-mode) "org-mode")
;;     (org-agenda-file-to-front)))
;; (add-hook 'after-save-hook 'to-agenda-on-save-org-mode-file)

;; Enable Confluence export
;; (require 'ox-confluence)


;;; [ Science ]

;;; [ Chemistry ]

;;; Chemistry: SMILES
(use-package smiles-mode
  :ensure t)
(use-package ob-smiles
  :ensure t)


;;; change Babel src block background color.

(setq org-src-fontify-natively t)

;; (setq org-src-block-faces
;;       '(("org" (:background (color-darken-name (face-background 'default) 4)))
;;         ("latex" (:background "#282828"))
;;         ("emacs-lisp" (:background "#202020"))
;;         ("lisp" (:background "#202020"))
;;         ("scheme" (:background "#202020"))
;;         ("clojure" (:background "#202020"))
;;         ("sh" (:background "#202020"))
;;         ("python" (:background "#202020"))
;;         ("ipython" (:background "CadetBlue"))
;;         ("ruby" (:background "#202020"))
;;         ("perl" (:background "#202020"))
;;         ("php" (:background "#202020"))
;;         ("C" (:background "#202020"))
;;         ("C++" (:background "#202020"))
;;         ("java" (:background "#202020"))
;;         ("js" (:background "#202020"))
;;         ("javascript" (:background "#202020"))
;;         ("coffee" (:background "dark slate blue"))
;;         ("haskell" (:background "#3d4451"))
;;         ("ocaml" (:background "saddle brown"))
;;         ("sql" (:background "#005b5b"))
;;         ("sqlite" (:background "#005b5b"))
;;         ("R" (:background "#3d4451"))
;;         ("julia" (:background "#3d4451"))
;;         ("octave" (:background "#3d4451"))
;;         ("matlab" (:background "#3d4451"))
;;         ("gnuplot" (:background "#3d4451"))
;;         ("sclang" (:background "#3d4451"))
;;         ("ditaa" (:background "#222222"))
;;         ("dot" (:background "#222222"))
;;         ("plantuml" (:background "#222222"))
;;         ("ledger" (:background "#222222"))
;;         ("calc" (:background "#222222"))
;;         ))


;; beacon effect when open org-mode babel src block editing.
(defun my-org-src-edit-animation ()
  (interactive)
  (let ((beacon-size 30)
        (beacon-color "violet red"))
    (beacon-blink)))

(add-hook 'org-src-mode-hook #'my-org-src-edit-animation)

;;; [ ob-async ] -- enables asynchronous execution of org-babel src blocks for *any* languages.

(use-package ob-async
  :ensure t
  :config
  (with-eval-after-load 'org
    (add-hook 'org-ctrl-c-ctrl-c-hook #'ob-async-org-babel-execute-src-block))

  ;; add header argument `:async' to default header arguments list
  ;; (add-to-list 'org-babel-default-header-args '(:async)) ; not work for Clojure Babel.

  (add-to-list 'org-babel-default-header-args:sh
               '(:async))
  (add-to-list 'org-babel-default-header-args:js
               '(:async))
  )

;;; [ org-babel-eval-in-repl ] -- eval org-babel block code with eval-in-repl.el

(use-package org-babel-eval-in-repl
  :ensure t
  :bind (:map org-mode-map
              ("C-<return>" . ober-eval-in-repl)
              ("C-c C-<return>" . ober-eval-block-in-repl))
  :config
  (with-eval-after-load "eval-in-repl"
    (setq eir-jump-after-eval nil))
  )

;;; [ ob-sql-mode ] -- SQL code blocks evaluated by sql-mode.

(use-package ob-sql-mode
  :ensure t
  :config
  (setq org-babel-sql-mode-template-selector "Q")
  ;; security guard
  (setq org-confirm-babel-evaluate
        (lambda (lang body)
          ;; (not )
          (string= lang "sql-mode")
          ))
  ;; (setq org-babel-default-header-args:sql-mode )
  )

;;; [ ob-uart ] -- A wrapper around make-serial-process for org babel, providing integration of UART communication into org documents.

(use-package ob-uart
  :ensure t
  :config
  ;; (org-babel-do-load-languages 'org-babel-load-languages '((uart . t)))
  )


(provide 'init-my-org-babel)

;;; init-my-org-babel.el ends here
