;;; init-my-org-babel.el --- init for Org Babel
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


;;;_* Babel
;;
;; - [C-c C-v] :: keymap prefix for babel. `org-babel-map'


(setq org-confirm-babel-evaluate nil)
(setq org-babel-no-eval-on-ctrl-c-ctrl-c nil)
(setq org-confirm-shell-link-function 'yes-or-no-p)
(setq org-confirm-elisp-link-function 'yes-or-no-p)

;;; source block header arguments
(setq org-babel-default-header-args    ; #+BEGIN_SRC ruby :result [output/value]
      '((:session . "none")
        (:results . "replace output")
        (:exports . "both")
        (:cache . "no")
        (:hlines . "no")
        (:padnewline . "yes")
        (:noweb . "no")
        (:tangle . "no")
        (:mkdirp . "yes")
        (:padline . "true")
        (:comments . "links")
        ))

;;; inline source code header arguments
(setq org-babel-default-inline-header-args
      '((:session . "none")
        (:results . "replace")
        (:exports . "both")
        (:hlines . "yes")
        ))

(setq org-src-fontify-natively t
      ;; nil: preserve org indent, t: preserve export indent.
      org-src-preserve-indentation nil
      org-edit-src-content-indentation 0
      org-src-tab-acts-natively t ; make [Tab] work native as in major mode.
      org-src-window-setup 'reorganize-frame ; 'current-window
      org-src-ask-before-returning-to-edit-buffer nil
      org-edit-src-auto-save-idle-delay 0 ; 0: don't auto save.
      )

;;; babel eval result
(setq org-babel-inline-result-wrap "=%s="
      org-babel-hide-result-overlays nil
      ;; org-babel-results-keyword "RESULTS"
      org-export-babel-evaluate t
      )


;;; [ ob-processing ]
(require 'ob-processing)

;;; [ ob-gnuplot ]
(require 'ob-gnuplot)

;; [ ob-ipython ]
(use-package ob-ipython
  :ensure t
  :config
  ;; open ipython block block with `python-mode'
  (add-to-list 'org-src-lang-modes '("ipython" . python))
  
  ;; use IJulia backend for IPython notebook
  ;; (setq ob-ipython-kernel-extra-args "--profile julia")
  ;; (add-to-list 'org-src-lang-modes '("ipython" . julia))
  )

;;; [ ob-coq ]
(require 'ob-coq)

;;; [ ob-R ]
(require 'ob-R)

;;; [ ob-julia ]
(if (not (boundp 'inferior-julia-program-name))
    (setq inferior-julia-program-name "julia"))
;; (setq org-babel-julia-command "julia")

(use-package ess
  :ensure t
  :config
  (require 'ess-site))

(require 'ob-julia)
(setq org-babel-default-header-args:julia
      '((:results . "output replace")
        (:padnewline . "yes")))
(add-to-list 'org-src-lang-modes '("julia" . ess-julia))

;; [ ob-sql ]
;; (require 'ob-sql)

;; [ ob-sqlite ]
;; (require 'ob-sqlite)


(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)                     ; Emacs Lisp
   (org . t)                            ; Org-mode
   (sh . t)                             ; Shell
   (shell . t)                          ; Shell Script
   (makefile . t)                       ; Make
   (ruby . t)                           ; Ruby
   (python . t)                         ; Python
   (perl . t)                           ; Perl
   (C . t)                              ; C
   (java . t)                           ; Java
   (awk . t)                            ; Awk
   (sed . t)                            ; Sed
   (screen . t)                         ; Screen
   (lisp . t)                           ; Lisp
   (scheme . t)                         ; Scheme
   ;; (picolisp . t)                       ; Pico Lisp
   (clojure . t)                        ; Clojure
   (haskell . t)                        ; Haskell
   (scala . t)                          ; Scala
   ;; (io . t)                             ; IO
   ;; (J . t)                              ; J
   (ocaml . t)                          ; Objective Caml
   (js . t)                             ; JavaScript
   (css . t)                            ; CSS
   (latex . t)                          ; LaTeX
   (sql . t)                            ; SQL
   (sqlite . t)                         ; SQLite
   ;; (matlab . t)                         ; MATLAB
   (octave . t)                         ; Octave
   (gnuplot . t)                        ; gnuplot
   ;; (fortran . t)                        ; Fortran
   (ditaa . t)                          ; ditaa
   (dot . t)                            ; Graphviz, Dot
   (plantuml . t)                       ; PlantUML
   ;; (ebnf2ps . t)                        ; ebnf2ps
   (calc . t)                           ; Calc
   (ledger . t)                         ; ledger support in Babel
   ;; (asymptote . t)                      ; Asymptote
   (sass . t)                           ; Sass
   ;; -- Extra --
   ;; use advice: `org-babel-execute-src-block' to load language support lazily.
   ;; (C++ . t)                            ; C++
   (R . t)                              ; R
   ;; (go . t)
   ;; (ipython . t)
   ;; (restclient . t)                     ; ob-restclient
   ))


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


(add-to-list 'org-src-lang-modes (cons "SQL" 'sql))

;; TODO: js2-mode syntax highlighting not work in Org-mode Babel.
(with-eval-after-load 'js2-mode
  (add-to-list 'org-src-lang-modes '("js" . js2)))

;; open ruby source block with `enh-ruby-mode' major-mode.
;; FIXME: this caused org babel block syntax highlighting failed.
;; (add-to-list 'org-src-lang-modes '("ruby" . enh-ruby))
;; (add-to-list 'org-src-lang-modes (cons "ruby" 'enh-ruby))

(with-eval-after-load 'web-mode
  (add-to-list 'org-src-lang-modes '("rhtml" . web)))


;;;_ + ditaa & PlantUML & Graphviz

;; Org-babel makes it easy to generate decent graphics using external packages
;; like ditaa, graphviz, PlantUML, and others.
;;
;; The setup is really easy. ditaa is provided with the org-mode source. You'll
;; have to install the `graphviz' and `PlantUML' packages on your system.

;; ditaa & PlantUML setup
(setq org-ditaa-jar-path "~/.emacs.d/init/extra/ditaa0_9.jar")
(setq org-plantuml-jar-path "~/.emacs.d/init/extra/plantuml.jar")

;; (add-hook 'org-mode-hook 'org-display-inline-images)
;; (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
;; (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images 'append)

;; (add-hook 'org-babel-after-execute-hook 'my/redisplay-inline-images 'append)
;; (defun my/redisplay-inline-images ()
;;   (condition-case nil
;;       (org-redisplay-inline-images)
;;     (error nil)))

;;; PlantUML language reference
;;
;; [[file:~/.emacs.d/init/extra/PlantUML%20Language%20Reference%20Guide.pdf][PlantUML Language Reference Guide]]

;; Use fundamental mode when editing plantuml blocks with C-c '
;; TODO: test whether I need to use a specific mode.
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

(setq org-babel-default-header-args:R
      '((:session . "*R*")
        (:exports . "both")
        (:results . "replace")
        ))

(setq org-babel-default-header-args:sqlite
      '((:db . "temp.db")
        (:results . "raw")
        ;; (:echo . t)
        (:column . t)
        (:nullvalue . "Null")))

;;; [ ob-ledger ]

(use-package ledger-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.ledger\\'" . ledger-mode))
  )

(use-package flycheck-ledger
  :ensure t
  :config
  (add-hook 'ledger-mode-hook
            (lambda ()
              (flycheck-mode 1)
              (flycheck-select-checker 'ledger)))
  )

(setq org-capture-templates
      (append '(("l" "Ledger entries")
                ;; Expense
                ("le" "Expenses")
                ("les" "Shopping" plain
                 (file (concat org-directory "/Accounting/my.ledger"))
                 "%(org-read-date) %^{Event}\n expenses:shopping:%^{category}  %^{Amount}")
                ("lef" "Food" plain
                 (file (concat org-directory "/Accounting/my.ledger"))
                 "%(org-read-date) %^{Event}\n expenses:food:%^{meat,breakfast,lunch,dinner}  %^{Amount}")
                ("let" "Traffic" plain
                 (file (concat org-directory "/Accounting/my.ledger"))
                 "%(org-read-date) %^{Event}\n expenses:traffic:%^{bus,train,plane}  %^{Amount}")
                ("leh" "House Rent" plain
                 (file (concat org-directory "/Accounting/my.ledger"))
                 "%(org-read-date) %^{Event}\n expenses:house rent:  %^{Amount}")
                
                ;; Income
                ("li" "Income")
                ("lis" "Salary" plain
                 (file (concat org-directory "/Accounting/my.ledger"))
                 "%(org-read-date) %^{Event}\n income:salary:%^{account}  %^{Amount}")

                ;; Transfer
                ("lt" "Transfer")
                ("ltb" "Take out money from Bank"
                 (file (concat org-directory "/Accounting/my.ledger"))
                 "%(org-read-date) %^{Event}\n transfer:%^{source} -> %^{bank}  %^{Amount}"
                 )
                ("lto" "save moeny on online account"
                 (file (concat org-directory "/Accounting/my.ledger"))
                 "%(org-read-date) %^{Event}\n transfer:%^{source} -> %^{ZhiFuBao}  %^{Amount}"
                 )
                ("ltc" "take out moeny to Cash"
                 (file (concat org-directory "/Accounting/my.ledger"))
                 "%(org-read-date) %^{Event}\n transfer:%^{source} -> cash  %^{Amount}"
                 )

                ;; Debt
                ("ld" "Debt")
                ("ldr" "Rent" plain
                 (file (concat org-directory "/Accounting/my.ledger"))
                 "%(org-read-date) %^{Event}\n debt:rent:%^{people}  %^{Amount}")
                ("ldb" "Borrow" plain
                 (file (concat org-directory "/Accounting/my.ledger"))
                 "%(org-read-date) %^{Event}\n debt:borrow:%^{people}  %^{Amount}")
                
                ;; Assets
                ("la" "Assets")
                ("lab" "Bank" plain
                 (file (concat org-directory "/Accounting/my.ledger"))
                 "%(org-read-date) %^{Event}\n assets:bank:%^{bank}  %^{Amount}")
                ("lao" "Online Accounts" plain
                 (file (concat org-directory "/Accounting/my.ledger"))
                 "%(org-read-date) %^{Event}\n assets:online-account:%^{ZhiFuBao}  %^{Amount}")
                )
              org-capture-templates))


;;; [ Library of Babel ]

;; (setq org-babel-lob-files
;;       org-babel-library-of-babel
;;       )


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


(provide 'init-my-org-babel)

;;; init-my-org-babel.el ends here
