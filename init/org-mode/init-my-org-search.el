;;; init-my-org-search.el --- init for Org search.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(setq org-occur-case-fold-search 'smart)

;;; [ Agenda dispatcher ]

;; (org-agenda-dispatch)

;;; [ org-seek ] -- Searching Org-mode files in directory.

(use-package org-seek
  :ensure t
  :config
  (define-key my-org-prefix (kbd "s") 'org-seek-headlines)
  (define-key my-org-prefix (kbd "S") 'org-seek-string)
  (define-key my-org-prefix (kbd "M-s") 'org-seek-regexp)
  )

;;; [ helm-org-rifle ] -- Rifle through your Org buffers and acquire your target.

(use-package helm-org-rifle
  :ensure t
  :config
  (setq helm-org-rifle-show-path t
        helm-org-rifle-fontify-headings t
        helm-org-rifle-show-todo-keywords t
        helm-org-rifle-show-tags t
        helm-org-rifle-directories-recursive t)
  
  (unless (boundp 'org-rifle-prefix)
    (define-prefix-command 'org-rifle-prefix))
  (define-key my-org-prefix (kbd "g") 'org-rifle-prefix)

  (define-key org-rifle-prefix (kbd "g") 'helm-org-rifle-current-buffer)
  (define-key org-rifle-prefix (kbd "G") 'helm-org-rifle)
  (define-key org-rifle-prefix (kbd "d") 'helm-org-rifle-directories)
  (define-key org-rifle-prefix (kbd "f") 'helm-org-rifle-files)
  (define-key org-rifle-prefix (kbd "o") 'helm-org-rifle-org-directory)
  (define-key org-rifle-prefix (kbd "a") 'helm-org-rifle-agenda-files)

  ;; LaTeX fragments preview support
  ;; FIXME: `org-toggle-latex-fragment' does not support in *non-file* buffer.
  ;; (add-hook 'helm-org-rifle-after-init-hook #'org-toggle-latex-fragment)
  
  ;; quick references searching.
  (unless (boundp 'references-rifle-prefix)
    (define-prefix-command 'references-rifle-prefix))
  (define-key my-org-prefix (kbd "r") 'references-rifle-prefix)

  ;; write a helper function for recursive searching org files.
  (defun my-helm-org-rifle-reference-search (&optional dirs-or-files)
    "A macro to quick recursive search through directory or files for Org-mode references."
    (interactive)
    (let* ((directories (or dirs-or-files
                            (-select 'f-dir?
                                     (helm-read-file-name "Directories: "
                                                          :marked-candidates t))))
           (files (f-files
                   directories
                   (lambda (file)
                     (s-matches?
                      helm-org-rifle-directories-filename-regexp
                      (f-filename file)))
                   t)))
      (if files
          (helm-org-rifle-files files)
        (error "No org files found in directories: %s" (s-join " " directories))))
    )

  (define-key references-rifle-prefix (kbd "r") 'my-helm-org-rifle-reference-search)

  (defvar my-org-rifle-references-common-path-languages
    (concat org-directory
            "/Wiki/Computer Technology/Programming/Programming Languages/"))
  
  ;; TeX/LaTeX
  (defun my-org-rifle-TeX-reference ()
    (interactive)
    (let ((my-tex-symbols-reference-dir
           (concat my-org-rifle-references-common-path-languages
                   "TeX/Data/Manuals/My TeX Symbols Reference/")))
      (helm-org-rifle-directories
       (list my-tex-symbols-reference-dir))))
  
  (define-key references-rifle-prefix (kbd "t") 'my-org-rifle-TeX-reference)

  ;; TEST:
  ;; (helm-org-rifle-directories
  ;;  (list (concat my-org-rifle-references-common-path-languages
  ;;                "Clojure/Data/Manuals/My Clojure Language Syntax Reference/")))
  
  ;; Clojure
  (defun my-org-rifle-Clojure-reference ()
    (interactive)
    (let ((my-clojure-syntax-reference-dir
           (concat my-org-rifle-references-common-path-languages
                   "Clojure/Data/Manuals/My Clojure Language Syntax Reference/")))
      (helm-org-rifle-directories
       (list my-clojure-syntax-reference-dir))))
  
  (define-key references-rifle-prefix (kbd "c") 'my-org-rifle-Clojure-reference)

  ;; Ruby
  (defun my-org-rifle-Ruby-reference ()
    (interactive)
    (let ((my-ruby-syntax-reference-dir
           (concat my-org-rifle-references-common-path-languages
                   "Ruby/Data/Manuals/Ruby Language Syntax Reference/")))
      (helm-org-rifle-directories
       (list my-ruby-syntax-reference-dir))))
  
  (define-key references-rifle-prefix (kbd "R") 'my-org-rifle-Ruby-reference)

  ;; Python
  (defun my-org-rifle-Python-reference ()
    (interactive)
    (let ((my-python-syntax-reference-dir
           (concat my-org-rifle-references-common-path-languages
                   "Python/Data/Manuals/My Python Language Syntax Reference/")))
      (helm-org-rifle-directories
       (list my-python-syntax-reference-dir))))
  
  (define-key references-rifle-prefix (kbd "p") 'my-org-rifle-Python-reference)
  
  ;; C
  (defun my-org-rifle-C-reference ()
    (interactive)
    (let ((my-C-syntax-reference-dir
           (concat my-org-rifle-references-common-path-languages
                   "C/Data/Manuals/My C Language Syntax Reference/")))
      (helm-org-rifle-directories
       (list my-C-syntax-reference-dir))))
  
  (define-key references-rifle-prefix (kbd "C") 'my-org-rifle-C-reference)
  
  ;; JavaScript
  (defun my-org-rifle-JavaScript-reference ()
    (interactive)
    (let ((my-javascript-syntax-reference-dir
           (concat my-org-rifle-references-common-path-languages
                   "JavaScript/Data/Manuals/My JavaScript Language Syntax Reference/")))
      (helm-org-rifle-directories
       (list my-javascript-syntax-reference-dir))))
  
  (define-key references-rifle-prefix (kbd "j") 'my-org-rifle-JavaScript-reference)

  ;; HTML
  (defun my-org-rifle-HTML-reference ()
    (interactive)
    (let ((my-html-syntax-reference-dir
           (concat my-org-rifle-references-common-path-languages
                   "HTML/Data/Manuals/My HTML Language Syntax Reference/")))
      (helm-org-rifle-directories
       (list my-html-syntax-reference-dir))))
  
  (define-key references-rifle-prefix (kbd "h") 'my-org-rifle-HTML-reference)

  ;; CSS
  (defun my-org-rifle-CSS-reference ()
    (interactive)
    (let ((my-css-syntax-reference-dir
           (concat my-org-rifle-references-common-path-languages
                   "CSS/Data/Manuals/My CSS Language Syntax Reference/")))
      (helm-org-rifle-directories
       (list my-css-syntax-reference-dir))))
  
  (define-key references-rifle-prefix (kbd "M-c") 'my-org-rifle-CSS-reference)

  ;; Julia
  (defun my-org-rifle-Julia-reference ()
    (interactive)
    (let ((my-julia-syntax-reference-dir
           (concat my-org-rifle-references-common-path-languages
                   "Julia/Data/Manuals/My Julia Language Syntax Reference/")))
      (helm-org-rifle-directories
       (list my-julia-syntax-reference-dir))))
  
  (define-key references-rifle-prefix (kbd "J") 'my-org-rifle-Julia-reference)

  ;; SQL
  (defun my-org-rifle-SQL-reference ()
    (interactive)
    (let ((my-sql-syntax-reference-dir
           (concat my-org-rifle-references-common-path-languages
                   "Database/SQL/Data/Manuals/My SQL Language Syntax Reference/")))
      (helm-org-rifle-directories
       (list my-sql-syntax-reference-dir))))
  
  (define-key references-rifle-prefix (kbd "s") 'my-org-rifle-SQL-reference)

  ;; Commands & Softwares
  (defun my-org-rifle-Commands-reference ()
    (interactive)
    (let ((my-commands-reference-dir
           (concat org-directory
                   "/Wiki/Computer Technology/Softwares/")))
      (helm-org-rifle-directories
       (list my-commands-reference-dir))))

  ;; NOTE: this is VERY SLOW!
  ;; (define-key references-rifle-prefix (kbd "M-s") 'my-org-rifle-Commands-reference)

  ;; Programming Implements
  (defun my-org-rifle-Implements-reference ()
    (interactive)
    (let ((my-implements-reference-dir
           (concat org-directory
                   "/Wiki/Computer Technology/Programming/Implements/")))
      (helm-org-rifle-directories
       (list my-implements-reference-dir))))
  
  (define-key references-rifle-prefix (kbd "i") 'my-org-rifle-Implements-reference)
  
  ;; Mathematics
  (defun my-org-rifle-Math-reference ()
    (interactive)
    (let ((my-math-formulas-reference-dir
           (concat org-directory
                   "/Wiki/Mathematics/Data/Manuals/Mathematics Formula Reference/"))
          (my-math-terms-reference-dir
           (concat org-directory
                   "/Wiki/Mathematics/Data/Manuals/Mathematics Terms Reference/")))
      (helm-org-rifle-directories
       (list my-math-formulas-reference-dir
             my-math-terms-reference-dir))))

  ;; NOTE: this is VERY SLOW!
  ;; (define-key references-rifle-prefix (kbd "m") 'my-org-rifle-Math-reference)

  ;; Bookmarks
  (defun my-org-rifle-bookmarks-reference ()
    (interactive)
    (let ((my-bookmarks-reference-dir (concat org-directory "/Bookmarks")))
      (helm-org-rifle-directories
       (list my-bookmarks-reference-dir))))

  (define-key references-rifle-prefix (kbd "C-b") 'my-org-rifle-bookmarks-reference)
  )

;;; [ helm-fuzzy-find ] -- [C-c C-/], [C-x c /]

(defun org-helm-fuzzy-find ()
  (interactive)
  (let ((default-directory org-directory))
    (helm-fuzzy-find nil)))

(define-key my-org-prefix (kbd "C-f") 'org-helm-fuzzy-find)


(provide 'init-my-org-search)

;;; init-my-org-search.el ends here
