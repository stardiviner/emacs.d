;;; init-my-org-babel.el --- init for Org Babel
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(setq org-confirm-babel-evaluate nil)
(setq org-babel-no-eval-on-ctrl-c-ctrl-c nil)
(setq org-confirm-shell-link-function 'yes-or-no-p)
(setq org-confirm-elisp-link-function 'yes-or-no-p)

(setq org-babel-uppercase-example-markers t)
(setq org-babel-hash-show-time t)

;;; source block header arguments
(setq org-babel-default-header-args
      '((:session . "none")
        (:results . "replace") (:cache . "no")
        (:exports . "code") (:hlines . "no")
        (:noweb . "no") (:tangle . "no")
        (:mkdirp . "yes")
        (:padline . "true") (:comments . "links")
        ))

;; (setq org-babel-default-inline-header-args)
;; (setq org-babel-default-lob-header-args)

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

(setq org-babel-load-languages
      '((org . t)                            ; Org-mode
        (shell . t)                          ; Shell Script
        (ditaa . t)                          ; ditaa
        (dot . t)                            ; Graphviz, Dot
        (plantuml . t)                       ; PlantUML
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
(setq org-babel-use-quick-and-dirty-noweb-expansion t)

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
;;
;; `org-babel-default-header-args:<lang>' where `<lang>' is the name of the
;; language.  See the language-specific documentation available online at
;; `http://orgmode.org/worg/org-contrib/babel'.


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


;;; [ Literate Programming with Org-mode ]

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

;; (use-package ob-async
;;   :ensure t
;;   :config
;;   ;; (add-to-list 'org-babel-default-header-args:shell
;;   ;;              '(:async))
;;   )

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

;; load all languages at last.
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)



(provide 'init-my-org-babel)

;;; init-my-org-babel.el ends here
