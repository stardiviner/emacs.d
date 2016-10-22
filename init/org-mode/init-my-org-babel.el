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

(setq org-src-fontify-natively t
      ;; nil: preserve org indent, t: preserve export indent.
      org-src-preserve-indentation nil
      ;; 0: fix `diff' babel syntax highlighting invalid issue.
      org-edit-src-content-indentation 0
      org-src-tab-acts-natively nil ; make [Tab] work native as in major mode.
      org-src-window-setup 'reorganize-frame ; 'current-window
      org-src-ask-before-returning-to-edit-buffer nil
      org-edit-src-auto-save-idle-delay 0 ; 0: don't auto save.
      )

;;; babel eval result
(setq org-babel-inline-result-wrap "=%s="
      org-export-babel-evaluate 'inline-only
      )

;;; [ ob-lisp ]
(setq org-babel-lisp-eval-fn #'sly-eval)

;;; [ ob-clojure ]
(setq org-babel-clojure-backend 'cider)

;;; [ ob-latex ]
(require 'ob-latex)

;;; [ ob-processing ]
(require 'ob-processing)

;;; [ ob-gnuplot ]
(require 'ob-gnuplot)

(setq org-babel-default-header-args:gnuplot
      '((:session . "none")
        (:results . "graphics")
        ;; (:dir . "data/images")
        ))

;; [ ob-ipython ]
(use-package ob-ipython
  :ensure t
  :config
  ;; (setq ob-ipython-command "ipython") ; "jupyter"
  
  ;; open ipython block block with `python-mode'
  ;; (add-to-list 'org-src-lang-modes '("ipython" . python))
  ;; use IJulia backend for IPython notebook
  (add-to-list 'org-src-lang-modes '("ipython" . julia))

  (setq org-babel-default-header-args:ipython
        '((:session . nil)
          ;; (:dir . "data/images")
          (:exports . "both")
          ))

  ;; different kernels support
  (defun ob-ipython-kernel-get-kernels ()
    "Get available Jupyter kernels.
This can be useful for snippets to select kernel interactively."
    (let ((kernels (split-string
                    (shell-command-to-string
                     "jupyter-kernelspec list | sed '1d' | awk -F ' ' '{print $1}'"))))
      ;; (completing-read "Jupyter kernels: "
      ;;                  kernels)
      kernels
      )
    )
  )

;;; [ ob-sagemath ]
(use-package ob-sagemath
  :ensure t)

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
   ;; (sass . t)                           ; Sass
   ;; -- Extra --
   ;; use advice: `org-babel-execute-src-block' to load language support lazily.
   ;; (C++ . t)                            ; C++
   ;; (D . t)                              ; D
   (R . t)                              ; R
   ;; (go . t)
   (ipython . t)
   (sagemath . t)        ; ob-sagemath
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

(add-to-list 'org-babel-default-header-args:clojure
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

(use-package ledger-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.ledger\\'" . ledger-mode))
  )

;; (setq org-babel-default-header-args:ledger
;;       '((:results . "output") (:cmdline . "bal"))
;;       )


(use-package flycheck-ledger
  :ensure t
  :defer t
  :init
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

(with-eval-after-load 'org
  ;; * Colored src blocks
  ;; based on patches from Rasmus <rasmus@gmx.us>

  ;; This function overwrites the org-src function to make src blocks be colored again.
  (defun org-src-font-lock-fontify-block (lang start end)
    "Fontify code block.
        LANG is the language of the block.  START and END are positions of
        the block.  This function is called by Emacs automatic
        fontification, as long as `org-src-fontify-natively' is non-nil."
    (let ((lang-mode (org-src--get-lang-mode lang)))
      (when (fboundp lang-mode)
        (let ((string (buffer-substring-no-properties start end))
              (modified (buffer-modified-p))
              (org-buffer (current-buffer))
              (block-faces (let ((face-name (intern (format "org-block-%s" lang))))
                             (append (and (facep face-name) (list face-name))
                                     '(org-block)))))
          (remove-text-properties start end '(face nil))
          (with-current-buffer
              (get-buffer-create
               (format " *org-src-fontification:%s*" lang-mode))
            (erase-buffer)
            (insert string " ") ;; so there's a final property change
            (unless (eq major-mode lang-mode) (funcall lang-mode))
            (org-font-lock-ensure)
            (let ((pos (point-min)) next)
              (while (setq next (next-single-property-change pos 'face))
                (let ((new-face (get-text-property pos 'face)))
                  (put-text-property
                   (+ start (1- pos)) (1- (+ start next)) 'face
                   (list :inherit (append (and new-face (list new-face))
                                          block-faces))
                   org-buffer))
                (setq pos next))
              ;; Add the face to the remaining part of the font.
              (put-text-property (1- (+ start pos))
                                 end 'face
                                 (list :inherit block-faces) org-buffer)))
          (add-text-properties
           start end
           '(font-lock-fontified t fontified t font-lock-multiline t))
          (set-buffer-modified-p modified)))))

  (defun org-fontify-meta-lines-and-blocks-1 (limit)
    "Fontify #+ lines and blocks."
    (let ((case-fold-search t))
      (if (re-search-forward
           "^\\([ \t]*#\\(\\(\\+[a-zA-Z]+:?\\| \\|$\\)\\(_\\([a-zA-Z]+\\)\\)?\\)[ \t]*\\(\\([^ \t\n]*\\)[ \t]*\\(.*\\)\\)\\)"
           limit t)
          (let ((beg (match-beginning 0))
                (block-start (match-end 0))
                (block-end nil)
                (lang (match-string 7))
                (beg1 (line-beginning-position 2))
                (dc1 (downcase (match-string 2)))
                (dc3 (downcase (match-string 3)))
                end end1 quoting block-type ovl)
            (cond
             ((and (match-end 4) (equal dc3 "+begin"))
              ;; Truly a block
              (setq block-type (downcase (match-string 5))
                    quoting (member block-type org-protecting-blocks))
              (when (re-search-forward
                     (concat "^[ \t]*#\\+end" (match-string 4) "\\>.*")
                     nil t)  ;; on purpose, we look further than LIMIT
                (setq end (min (point-max) (match-end 0))
                      end1 (min (point-max) (1- (match-beginning 0))))
                (setq block-end (match-beginning 0))
                (when quoting
                  (org-remove-flyspell-overlays-in beg1 end1)
                  (remove-text-properties beg end
                                          '(display t invisible t intangible t)))
                (add-text-properties
                 beg end '(font-lock-fontified t font-lock-multiline t))
                (add-text-properties beg beg1 '(face org-meta-line))
                (org-remove-flyspell-overlays-in beg beg1)
                (add-text-properties  ; For end_src
                 end1 (min (point-max) (1+ end)) '(face org-meta-line))
                (org-remove-flyspell-overlays-in end1 end)
                (cond
                 ((and lang (not (string= lang "")) org-src-fontify-natively)
                  (org-src-font-lock-fontify-block lang block-start block-end)
                  (add-text-properties beg1 block-end '(src-block t)))
                 (quoting
                  (add-text-properties beg1 (min (point-max) (1+ end1))
                                       (let ((face-name (intern (format "org-block-%s" lang))))
                                         (append (and (facep face-name) (list face-name))
                                                 '(face org-block))))) ; end of source block
                 ((not org-fontify-quote-and-verse-blocks))
                 ((string= block-type "quote")
                  (add-text-properties beg1 (min (point-max) (1+ end1)) '(face org-quote)))
                 ((string= block-type "verse")
                  (add-text-properties beg1 (min (point-max) (1+ end1)) '(face org-verse))))
                (add-text-properties beg beg1 '(face org-block-begin-line))
                (add-text-properties (min (point-max) (1+ end)) (min (point-max) (1+ end1))
                                     '(face org-block-end-line))
                t))
             ((member dc1 '("+title:" "+author:" "+email:" "+date:"))
              (org-remove-flyspell-overlays-in
               (match-beginning 0)
               (if (equal "+title:" dc1) (match-end 2) (match-end 0)))
              (add-text-properties
               beg (match-end 3)
               (if (member (intern (substring dc1 1 -1)) org-hidden-keywords)
                   '(font-lock-fontified t invisible t)
                 '(font-lock-fontified t face org-document-info-keyword)))
              (add-text-properties
               (match-beginning 6) (min (point-max) (1+ (match-end 6)))
               (if (string-equal dc1 "+title:")
                   '(font-lock-fontified t face org-document-title)
                 '(font-lock-fontified t face org-document-info))))
             ((equal dc1 "+caption:")
              (org-remove-flyspell-overlays-in (match-end 2) (match-end 0))
              (remove-text-properties (match-beginning 0) (match-end 0)
                                      '(display t invisible t intangible t))
              (add-text-properties (match-beginning 1) (match-end 3)
                                   '(font-lock-fontified t face org-meta-line))
              (add-text-properties (match-beginning 6) (+ (match-end 6) 1)
                                   '(font-lock-fontified t face org-block))
              t)
             ((member dc3 '(" " ""))
              (org-remove-flyspell-overlays-in beg (match-end 0))
              (add-text-properties
               beg (match-end 0)
               '(font-lock-fontified t face font-lock-comment-face)))
             (t ;; just any other in-buffer setting, but not indented
              (org-remove-flyspell-overlays-in (match-beginning 0) (match-end 0))
              (remove-text-properties (match-beginning 0) (match-end 0)
                                      '(display t invisible t intangible t))
              (add-text-properties beg (match-end 0)
                                   '(font-lock-fontified t face org-meta-line))
              t))))))


  (defface org-block-org
    `((t (:background ,(color-darken-name (face-background 'default) 4))))
    "Face for Org-mode src blocks")

  (defface org-block-emacs-lisp
    `((t (:background "#202020")))
    "Face for elisp src blocks")

  (defface org-block-lisp
    `((t (:background "#202020")))
    "Face for Common Lisp src blockss")

  (defface org-block-clojure
    `((t (:background "#202020")))
    "Face for Clojure blocks")

  (defface org-block-scheme
    `((t (:background "#3d4451")))
    "Face for Scheme src blockss")

  (defface org-block-sh
    `((t (:background "#3d4451")))
    "Face for shell blocks")

  (defface org-block-latex
    `((t (:background "#282828")))
    "Face for LaTeX blocks")
  
  (defface org-block-python
    `((t (:background "#202020")))
    "Face for python blocks")

  (defface org-block-ipython
    `((t (:background "CadetBlue")))
    "Face for IPython blocks")

  (defface org-block-ruby
    `((t (:background "#202020")))
    "Face for Ruby blocks")

  (defface org-block-perl
    `((t (:background "DarkSlateGray")))
    "Face for Ruby blocks")

  (defface org-block-C
    `((t (:background "DarkGreen")))
    "Face for C blocks")

  (defface org-block-C++
    `((t (:background "DarkGreen")))
    "Face for C++ blocks")

  (defface org-block-java
    `((t (:background "DarkOliveGreen")))
    "Face for Java blocks")

  (defface org-block-js
    `((t (:background "#202020")))
    "Face for JavaScript blocks")

  (defface org-block-javascript
    `((t (:background "#202020")))
    "Face for JavaScript blocks")
  
  (defface org-block-coffee
    `((t (:background "DarkSlateBlue")))
    "Face for CoffeeScript blocks")

  (defface org-block-haskell
    `((t (:background "Darkorchid4")))
    "Face for Haskell blocks")

  (defface org-block-ocaml
    `((t (:background "SaddleBrown")))
    "Face for OCaml blocks")

  (defface org-block-sql
    `((t (:background "#005b5b")))
    "Face for SQL blocks")

  (defface org-block-sqlite
    `((t (:background "#005b5b")))
    "Face for SQLite blocks")

  (defface org-block-R
    `((t (:background "MidnightBlue")))
    "Face for R blocks")

  (defface org-block-julia
    `((t (:background "MidnightBlue")))
    "Face for Julia blocks")
  
  (defface org-block-octave
    `((t (:background "MidnightBlue")))
    "Face for Octave blocks")

  (defface org-block-matlab
    `((t (:background "MidnightBlue")))
    "Face for Matlab blocks")
  
  (defface org-block-gnuplot
    `((t (:background "MidnightBlue")))
    "Face for gnuplot blocks")

  (defface org-block-sclang
    `((t (:background "MidnightBlue")))
    "Face for SuperCollider sclang blocks")

  (defface org-block-ditaa
    `((t (:background "DimGray")))
    "Face for Ditaa blocks")

  (defface org-block-dot
    `((t (:background "DimGray")))
    "Face for Dot blocks")
  
  (defface org-block-plantuml
    `((t (:background "DimGray")))
    "Face for plantuml blocks")

  (defface org-block-ledger
    `((t (:background "DimGray")))
    "Face for ledger blocks")

  (defface org-block-calc
    `((t (:background "DimGray")))
    "Face for calc blocks")
  )


(provide 'init-my-org-babel)

;;; init-my-org-babel.el ends here
