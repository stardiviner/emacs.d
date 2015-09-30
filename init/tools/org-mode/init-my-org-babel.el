;;; init-my-org-babel.el --- init for Org Babel
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


;;;_* Babel
;;
;; - [C-c C-v] :: keymap prefix for babel. `org-babel-map'


(setq org-confirm-babel-evaluate t)     ; org-babel-evaluate confirm.
(setq org-babel-no-eval-on-ctrl-c-ctrl-c nil)
(setq org-confirm-shell-link-function 'yes-or-no-p)
(setq org-confirm-elisp-link-function 'yes-or-no-p)

;;; source block header arguments
(setq org-babel-default-header-args    ; #+BEGIN_SRC ruby :result [output/value]
      '((:session . "none")
        (:results . "replace output") ; "replace output", "replace", "file", "output"
        (:exports . "both")           ; "both", "results", "code" "none"
        (:cache . "no")
        (:noweb . "no")
        (:hlines . "no")
        (:padnewline . "yes")
        ;; for tangle
        (:tangle . "no")
        ;; for tangle jump between org <-> code file
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


(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)                     ; Emacs Lisp
   (org . t)                            ; Org-mode
   (shell . t)                          ; Shell Script
   (sh . t)                             ; Shell
   (makefile . t)                       ; Make
   (ruby . t)                           ; Ruby
   (python . t)                         ; Python
   ;; (perl . t)                           ; Perl
   (C . t)                              ; C
   ;; (C++ . t)                            ; C++
   (java . t)                           ; Java
   ;; (awk . t)                            ; Awk
   ;; (sed . t)                            ; Sed
   ;; (screen . t)                         ; Screen
   (lisp . t)                           ; Lisp
   ;; (scheme . t)                         ; Scheme
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
   ;; (R . t)                              ; R
   (sql . t)                            ; SQL
   (sqlite . t)                         ; SQLite
   (matlab . t)                         ; MATLAB
   (octave . t)                         ; Octave
   (gnuplot . t)                        ; gnuplot
   ;; (fortran . t)                        ; Fortran
   (ditaa . t)                          ; ditaa
   (dot . t)                            ; Graphviz, Dot
   (plantuml . t)                       ; PlantUML
   ;; (ebnf2ps . t)                        ; ebnf2ps
   (calc . t)                           ; Calc
   ;; (ledger . t)                         ; ledger support in Babel
   ;; (asymptote . t)                      ; Asymptote
   ;; (sass . t)                           ; Sass
   ))

(setq
 org-babel-tangle-lang-exts '(("latex" . "tex")
                              ("elisp" . "el")
                              ("emacs-lisp" . "el")
                              ("lisp" . "lisp")
                              ("ruby" . "rb")
                              ("python" . "py")
                              ;; ("R" . "R")
                              ("sql" . "sql")
                              ("sh" . "sh")
                              ;; ("haskell" . "hs")
                              ("clojure" . "clj")
                              ;; ("awk" . "awk")
                              ("C" . "c")
                              ("Go" . "go")
                              ("C++" . "cpp")
                              ("perl" . "pl")
                              ("js" . "js")
                              ("css" . "css")
                              ("java" . "java")
                              )
 ;; org-babel-pre-tangle-hook '(save-buffer)
 )



;; open ruby source block with `enh-ruby-mode' major-mode.
(with-eval-after-load 'enh-ruby-mode
  (add-to-list 'org-src-lang-modes '("ruby" . enh-ruby)))


;;; [ ob-processing ]

(require 'ob-processing)

;;; [ ob-julia ]
;;
;; this require "ESS"
;;
;; TODO: read https://github.com/gjkerns/ob-julia/blob/master/ob-julia-doc.org

(if (not (boundp 'inferior-julia-program-name))
    (setq inferior-julia-program-name "julia"))

(require 'ob-julia)

(setq org-babel-default-header-args:julia
      '((:results . "replace output")
        (:padnewline . "yes")))

;; [ ob-mongo ] -- babel for MongoDB

(require-package 'ob-mongo)

;; [ ob-sql ]

(require 'ob-sql)

;; [ ob-go ]
;;
;; Usage:
;;
;; * simple example:
;;
;; #+BEGIN_SRC go :imports "fmt"
;; fmt.Println("Hello, World")
;; #+END_SRC
;;
;; #+results:
;; : Hello, World
;;
;; * multiple imports
;;
;; #+BEGIN_SRC go :imports '("fmt" "time")
;; fmt.Println("Current Time:", time.Now())
;; #+END_SRC
;;
;; #+RESULTS:
;; : Current Time: 2015-05-23 13:02:50.87256801 +0800 CST
;;
;; * concurrent prime sieve
;;
;; #+begin_src go
;;   // A concurrent prime sieve
;;   package main
;;
;;   import "fmt"
;;
;;   // Send the sequence 2, 3, 4, ... to channel 'ch'.
;;   func Generate(ch chan<- int) {
;;           for i := 2; ; i++ {
;;                   ch <- i // Send 'i' to channel 'ch'.
;;           }
;;   }
;;
;;   // Copy the values from channel 'in' to channel 'out',
;;   // removing those divisible by 'prime'.
;;   func Filter(in <-chan int, out chan<- int, prime int) {
;;           for {
;;                   i := <-in // Receive value from 'in'.
;;                   if i%prime != 0 {
;;                           out <- i // Send 'i' to 'out'.
;;                   }
;;           }
;;   }
;;
;;   // The prime sieve: Daisy-chain Filter processes.
;;   func main() {
;;           ch := make(chan int) // Create a new channel.
;;           go Generate(ch)      // Launch Generate goroutine.
;;           for i := 0; i < 10; i++ {
;;                   prime := <-ch
;;                   fmt.Println(prime)
;;                   ch1 := make(chan int)
;;                   go Filter(ch, ch1, prime)
;;                   ch = ch1
;;           }
;;   }
;; #+end_src
;;
;; #+RESULTS:
;; #+begin_example
;;   2
;;   3
;;   5
;;   7
;;   11
;;   13
;;   17
;;   19
;;   23
;;   29
;; #+end_example
;;


;; (require-package 'ob-go)

;; [ ob-prolog ] -- babel for Prolog

(require-package 'ob-prolog)

;; [ ob-http ] -- http request in org-mode babel
;;
;; | option      | curl         | example                                                                               |
;; |-------------+--------------+---------------------------------------------------------------------------------------|
;; | :proxy      | -x           | :proxy localhost:8118                                                                 |
;; | :cookie-jar | --cookie-jar | :cookie-jar username                                                                  |
;; | :cookie     | --cookie     | :cookie username                                                                      |
;; | :max-time   | --max-time   | default is 10                                                                         |
;; | :pretty     | N/A          | :pretty use Content-Type, currently only json is supported, to overwrite :pretty json |
;; | :select     | N/A          | :select .path path will be passed to jq                                               |
;; | :get-header | N/A          | :get-header X-Subject-Token                                                           |


(require-package 'ob-http)

;;; [ ob-browser ] -- render HTML in org babel

;; #+BEGIN_SRC browser :out output.png
;; <!DOCTYPE html>
;; <html>
;;   <body>
;;     <p>hello, world</p>
;;   </body>
;; </html>
;; #+END_SRC

(require-package 'ob-browser)

;; open those babels with `web-mode'.
(eval-after-load "web-mode"
  '(progn
     (add-to-list 'org-src-lang-modes '("html" . web))
     (add-to-list 'org-src-lang-modes '("browser" . web))
     (add-to-list 'org-src-lang-modes '("rhtml" . web))
     ))



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
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

(add-hook 'org-babel-after-execute-hook 'my/redisplay-inline-images 'append)
(defun my/redisplay-inline-images ()
  (condition-case nil
      (org-redisplay-inline-images)
    (error nil)))

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
;; TODO:
;; (setq org-babel-default-header-args:latex
;;       '(;; generate results as #+BEGIN_LaTeX ... #+END_LaTeX block.
;;         ;; (:results . "latex")
;;         ;; (:exports . "results")
;;         ;; generate result as a (bitmap) image or pdf.
;;         ;; (:file . "temp.png")
;;         ))


(setq org-babel-default-header-args:R
      '((:session . "no")
        (:exports . "both")
        (:results . "replace")
        ))

(setq org-babel-default-header-args:sqlite
      '((:db . "temp.db")
        (:results . "raw")
        ;; (:echo . t)
        (:column . t)
        (:nullvalue . "Null")))


;;;_ * Library of Babel

;;; Usage:
;;
;; - `org-babel-lob-ingest' [C-c C-v i]
;;    Add all named source blocks defined in FILE to `org-babel-library-of-babel'.

;; Files used to populate the `org-babel-library-of-babel'.
;; To add files to this list use the `org-babel-lob-ingest' command.
;; TODO:
;; (setq org-babel-lob-files
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


;;; Templates -- (org skeleton/template)

;; <s, <e, <q, ...

;;; append templates into this list variable.
;;
;; default structure easy template alist
;;
;; (setq org-structure-template-alist
;;       '(("s" "#+BEGIN_SRC ?\n\n#+END_SRC")
;;         ("e" "#+BEGIN_EXAMPLE\n?\n#+END_EXAMPLE")
;;         ("q" "#+BEGIN_QUOTE\n?\n#+END_QUOTE")
;;         ("v" "#+BEGIN_VERSE\n?\n#+END_VERSE")
;;         ("V" "#+BEGIN_VERBATIM\n?\n#+END_VERBATIM")
;;         ("c" "#+BEGIN_CENTER\n?\n#+END_CENTER")
;;         ("l" "#+BEGIN_LaTeX\n?\n#+END_LaTeX")
;;         ("L" "#+LaTeX: ")
;;         ("h" "#+BEGIN_HTML\n?\n#+END_HTML")
;;         ("H" "#+HTML: ")
;;         ("a" "#+BEGIN_ASCII\n?\n#+END_ASCII")
;;         ("A" "#+ASCII: ")
;;         ("i" "#+INDEX: ?")
;;         ("I" "#+INCLUDE: %file ?")))




(provide 'init-my-org-babel)

;;; init-my-org-babel.el ends here
