;;; init-org-search.el --- init for Org search.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(setq search-invisible t) ; search for Org Mode invisible text.

(setq org-occur-case-fold-search 'smart)

;;; org-agenda [C-c a s] / `org-search-view' search view.

;;; [ helm-org-rifle ] -- Rifle through your Org buffers and acquire your target.

(use-package helm-org-rifle
  :ensure t
  :defer t
  :init
  (unless (boundp 'org-rifle-prefix)
    (define-prefix-command 'org-rifle-prefix))
  (define-key Org-prefix (kbd "g") 'org-rifle-prefix)
  
  (define-key org-rifle-prefix (kbd "g") 'helm-org-rifle-current-buffer)
  (define-key org-rifle-prefix (kbd "G") 'helm-org-rifle)
  (define-key org-rifle-prefix (kbd "d") 'helm-org-rifle-directories)
  (define-key org-rifle-prefix (kbd "f") 'helm-org-rifle-files)
  (define-key org-rifle-prefix (kbd "a") 'helm-org-rifle-agenda-files)
  (define-key org-rifle-prefix (kbd "M-r") 'helm-org-rifle-org-directory)

  (setq helm-org-rifle-show-path t
        helm-org-rifle-fontify-headings t
        helm-org-rifle-show-todo-keywords t
        helm-org-rifle-show-tags t
        helm-org-rifle-directories-recursive t)

  ;; LaTeX fragments preview support
  ;; FIXME: `org-toggle-latex-fragment' does not support in *non-file* buffer.
  ;; (add-hook 'helm-org-rifle-after-init-hook #'org-toggle-latex-fragment)

  ;; quick references searching.
  (unless (boundp 'reference-prefix)
    (define-prefix-command 'reference-prefix))
  (define-key Org-prefix (kbd "r") 'reference-prefix)

  ;;=============================================================================
  
  ;; write a helper function for recursive searching org files.
  (defun Rifle-refs-search (&optional dirs-or-files)
    "A macro to quick recursive search through directory or files for Org-mode references."
    (interactive)
    (let* ((directories (or dirs-or-files
                            (-select 'f-dir?
                                     (helm-read-file-name "Directories: "
                                                          :marked-candidates t))))
           (files (f-files directories (lambda (file)
                                         (s-matches?
                                          helm-org-rifle-directories-filename-regexp
                                          (f-filename file)))
                           t)))
      (if files
          (helm-org-rifle-files files)
        (error "No org files found in directories: %s" (s-join " " directories)))))

  (define-key reference-prefix (kbd "SPC") 'Rifle-refs-search)

  ;;===============================================================================
  
  (defvar rifle-references-common-path--languages
    (concat org-directory "/Wiki/Computer Technology/Programming/Programming Languages/"))

  ;; Emacs modes
  (defun rifle-Emacs-modes-ref ()
    (interactive)
    (let ((my-emacs-modes-reference-dir
           (concat org-directory
                   "/Wiki/Computer Technology/Programming/Emacs/Data/Emacs Packages/")))
      (helm-org-rifle-directories (list my-emacs-modes-reference-dir))))
  ;; NOTE: this is VERY SLOW!
  ;; (define-key reference-prefix (kbd "e") 'rifle-Emacs-modes-ref)

  ;; TeX/LaTeX
  (defun rifle-TeX-ref ()
    (interactive)
    (let ((my-tex-symbols-reference-dir
           (concat rifle-references-common-path--languages
                   "TeX/Data/Manuals/My TeX Symbols Reference/")))
      (helm-org-rifle-directories (list my-tex-symbols-reference-dir))))
  (define-key reference-prefix (kbd "t") 'rifle-TeX-ref)
  
  ;; Clojure
  (defun rifle-Clojure-ref ()
    (interactive)
    (let ((my-clojure-syntax-reference-dir
           (concat rifle-references-common-path--languages
                   "Clojure/Data/Manuals/My Clojure Language Syntax Reference/")))
      (helm-org-rifle-directories (list my-clojure-syntax-reference-dir))))
  (define-key reference-prefix (kbd "c") 'rifle-Clojure-ref)

  ;; Ruby
  (defun rifle-Ruby-ref ()
    (interactive)
    (let ((my-ruby-syntax-reference-dir
           (concat rifle-references-common-path--languages
                   "Ruby/Data/Manuals/My Ruby Language Syntax Reference/")))
      (helm-org-rifle-directories (list my-ruby-syntax-reference-dir))))
  (define-key reference-prefix (kbd "r") 'rifle-Ruby-ref)

  ;; Python
  (defun rifle-Python-ref ()
    (interactive)
    (let ((my-python-syntax-reference-dir
           (concat rifle-references-common-path--languages
                   "Python/Data/Manuals/My Python Language Syntax Reference/")))
      (helm-org-rifle-directories (list my-python-syntax-reference-dir))))
  (define-key reference-prefix (kbd "p") 'rifle-Python-ref)
  
  ;; C
  (defun rifle-C-ref ()
    (interactive)
    (let ((my-C-syntax-reference-dir
           (concat rifle-references-common-path--languages
                   "C/Data/Manuals/My C Language Syntax Reference/")))
      (helm-org-rifle-directories (list my-C-syntax-reference-dir))))
  (define-key reference-prefix (kbd "C") 'rifle-C-ref)
  
  ;; JavaScript
  (defun rifle-JavaScript-ref ()
    (interactive)
    (let ((my-javascript-syntax-reference-dir
           (concat rifle-references-common-path--languages
                   "JavaScript/Data/Manuals/My JavaScript Language Syntax Reference/")))
      (helm-org-rifle-directories (list my-javascript-syntax-reference-dir))))
  (define-key reference-prefix (kbd "j") 'rifle-JavaScript-ref)

  ;; HTML
  (defun rifle-HTML-ref ()
    (interactive)
    (let ((my-html-syntax-reference-dir
           (concat rifle-references-common-path--languages
                   "HTML/Data/Manuals/My HTML Language Syntax Reference/")))
      (helm-org-rifle-directories (list my-html-syntax-reference-dir))))
  (define-key reference-prefix (kbd "h") 'rifle-HTML-ref)

  ;; CSS
  (defun rifle-CSS-ref ()
    (interactive)
    (let ((my-css-syntax-reference-dir
           (concat rifle-references-common-path--languages
                   "CSS/Data/Manuals/My CSS Language Syntax Reference/")))
      (helm-org-rifle-directories (list my-css-syntax-reference-dir))))
  (define-key reference-prefix (kbd "H") 'rifle-CSS-ref)

  ;; Julia
  (defun rifle-Julia-ref ()
    (interactive)
    (let ((my-julia-syntax-reference-dir
           (concat rifle-references-common-path--languages
                   "Julia/Data/Manuals/My Julia Language Syntax Reference/")))
      (helm-org-rifle-directories (list my-julia-syntax-reference-dir))))
  (define-key reference-prefix (kbd "J") 'rifle-Julia-ref)

  ;; SQL
  (defun rifle-SQL-ref ()
    (interactive)
    (let ((my-sql-syntax-reference-dir
           (concat rifle-references-common-path--languages
                   "Database/SQL/Data/Manuals/My SQL Language Syntax Reference/")))
      (helm-org-rifle-directories (list my-sql-syntax-reference-dir))))
  (define-key reference-prefix (kbd "S") 'rifle-SQL-ref)

  ;; PostgreSQL
  (defun rifle-PostgreSQL-ref ()
    (interactive)
    (let ((my-postgresql-syntax-reference-dir
           (concat rifle-references-common-path--languages
                   "Database/SQL/PostgresQL/Data/Manuals/My PostgreSQL Language Syntax Reference/")))
      (helm-org-rifle-directories (list my-postgresql-syntax-reference-dir))))
  (define-key reference-prefix (kbd "s") 'rifle-PostgreSQL-ref)

  ;; Commands & Softwares
  (defun rifle-Commands-ref ()
    (interactive)
    (let ((my-commands-reference-dir
           (concat org-directory
                   "/Wiki/Computer Technology/Softwares/")))
      (helm-org-rifle-directories (list my-commands-reference-dir))))
  ;; NOTE: this is VERY SLOW!
  ;; (define-key reference-prefix (kbd "M-s") 'rifle-Commands-ref)

  ;; Programming Implements
  (defun rifle-Implementations-ref ()
    (interactive)
    (let ((my-implements-reference-dir
           (concat org-directory
                   "/Wiki/Computer Technology/Programming/Implementations/")))
      (helm-org-rifle-directories (list my-implements-reference-dir))))
  ;; NOTE very slow!!!
  ;; (define-key reference-prefix (kbd "i") 'rifle-Implementations-ref)

  ;; Computer Science Glossary
  (defun rifle-Computer-Science-ref ()
    (interactive)
    (let ((my-implements-reference-dir
           (concat org-directory
                   "/Wiki/Computer Technology/Computer Science/Data/Manuals/Computer Science Glossary/")))
      (helm-org-rifle-directories (list my-implements-reference-dir))))
  (define-key reference-prefix (kbd "M-c") 'rifle-Computer-Science-ref)
  
  ;; Mathematics
  (defun rifle-Math-ref ()
    (interactive)
    (let ((my-math-formulas-reference-dir
           (concat org-directory
                   "/Wiki/Mathematics/Data/Manuals/Mathematics Formula Reference/"))
          (my-math-terms-reference-dir
           (concat org-directory
                   "/Wiki/Mathematics/Data/Manuals/Mathematics Terms Reference/")))
      (helm-org-rifle-directories
       (list my-math-formulas-reference-dir my-math-terms-reference-dir))))
  ;; NOTE: this is VERY SLOW!
  ;; (define-key reference-prefix (kbd "m") 'rifle-Math-ref)

  ;; Bookmarks
  (defun rifle-bookmarks-ref ()
    (interactive)
    (let ((my-bookmarks-reference-dir (concat org-directory "/Bookmarks")))
      (helm-org-rifle-directories (list my-bookmarks-reference-dir))))
  (define-key reference-prefix (kbd "C-b") 'rifle-bookmarks-ref))

;; search multiple words in files.
(use-package rg
  :ensure t
  :defer t
  :init
  (defun rg-files-with-matches-beginning (dir file-type word)
    "Construct literal rg comdnam end part for the beginning WORD in DIR with FILE-TYPE."
    (format "rg --files-with-matches --null --type %s -e \"%s\" %s" file-type word dir))

  (defun rg-files-with-matches-middle (word)
    "Construct literal rg comdnam end part for the middle WORD."
    (format " | xargs --null rg --files-with-matches --null -e \"%s\"" word))

  (defun rg-files-with-matches-end (word)
    "Construct literal rg comdnam end part for the last WORD."
    (format " | xargs --null rg --heading -e \"%s\"" word))

  (defun rg-files-construct-command (dir)
    "Construct a literal rg command to search WORDS in DIR."
    (let* ((words (split-string (read-from-minibuffer "Words: ") " "))
           (file-type (completing-read "file type: "
                                       (mapcar 'car (rg-get-type-aliases))
                                       nil nil "org"))
           (file-type-exts (assq file-type (rg-get-type-aliases))))
      (case (length words)
        (1 (format "rg %s" (car words)))
        (2 (concat (rg-files-with-matches-beginning dir file-type (car words))
                   (rg-files-with-matches-end (cdr words))))
        (3 (concat (rg-files-with-matches-beginning dir file-type (car words))
                   (rg-files-with-matches-middle (cadr words))
                   (rg-files-with-matches-end (car (reverse words)))))
        (t (concat (rg-files-with-matches-beginning dir file-type (car words))
                   ;; KLUDGE: Do I have to use (car (mapcan ...))?
                   (car (mapcan
                         (lambda (word) (list (rg-files-with-matches-middle word)))
                         (delq (car (reverse words)) (cdr words))))
                   (rg-files-with-matches-end (car (reverse words))))))))

  (defun rg-search-words-by-files (directory)
    "Search multiple words in files of DIRECTORY as unit instead of line.

The literal rg command looks like this:

rg --files-with-matches --null \"foo\" . | \\
xargs --null rg --files-with-matches --null \"bar\" | \\
... | \\
xargs --null rg --heading \"baz\"

That's it.
"
    ;; interactively select Org default directory or current directory.
    (interactive (list (completing-read "Dir: " `(,(expand-file-name org-directory)
                                                  ,default-directory))))
    (let* ((command (rg-files-construct-command (shell-quote-argument directory))))
      ;; FIXME rg report 0 match, but the result has matches.
      ;; dive into rg.el source code to figure out.
      ;; use `rg-define-search'
      (compilation-start command 'rg-mode)))

  (define-key Org-prefix (kbd "s") 'rg-search-words-by-files))

;;; [ org-recoll ] -- A lightweight Emacs Org Mode wrapper for the recoll full-text search engine.

;; (use-package org-recoll
;;   :quelpa (org-recoll :fetcher github :repo "alraban/org-recoll")
;;   :defer t
;;   :commands (org-recoll-search org-recoll-update-index)
;;   :bind (:map Org-prefix ("C-s" . org-recoll-search)
;;               :map org-recoll-mode-map
;;               ("M-n" . org-recoll-next-page)
;;               ("M-p" . org-recoll-previous-page)
;;               ("q" . delete-window))
;;   :init
;;   (add-to-list 'display-buffer-alist '("^\\*org-recoll-index\\*" . (display-buffer-below-selected)))
;;   (add-to-list 'display-buffer-alist '("^\\*org-recoll-results\\*" . (display-buffer-below-selected))))

;;; [ microfts ] -- Small and fast FTS (full text search) for Org Mode files.

;; (use-package org-fts
;;   :load-path "~/Code/Emacs/microfts/elisp"
;;   :commands (ivy-org-fts-search ivy-org-fts-find-org-file)
;;   :bind (:map Org-prefix ("C-s" . ivy-org-fts-search))
;;   :custom ((org-fts-program "~/Code/Emacs/microfts/microfts")
;;            (org-fts-db (file-truename (format "%sorg-fts.db" user-emacs-directory))))
;;   :init (require 'ivy-org-fts))

;;; [ org-ql ] -- An Org-mode query language, search command, and agenda-like view.

(use-package org-ql
  :ensure t
  :ensure helm-org-ql
  :demand t
  :requires (helm-org-ql)
  :commands (helm-org-ql org-ql-sparse-tree org-ql-search org-ql-view)
  :bind (([remap org-goto] . helm-org-ql)
         ;; :map org-mode-map ("M-s" . org-ql-search)
         )
  :init (add-to-list 'display-buffer-alist '("^\\*Org QL.*\\*" . (display-buffer-below-selected))))



(provide 'init-org-search)

;;; init-org-search.el ends here
