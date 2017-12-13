;;; init-my-org-search.el --- init for Org search.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(setq org-occur-case-fold-search 'smart)

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
  (define-key Org-prefix (kbd "g") 'org-rifle-prefix)

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
  (unless (boundp 'reference-prefix)
    (define-prefix-command 'reference-prefix))
  (define-key Org-prefix (kbd "r") 'reference-prefix)

  ;; write a helper function for recursive searching org files.
  (defun Rifle-refs-search (&optional dirs-or-files)
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

  (define-key reference-prefix (kbd "SPC") 'Rifle-refs-search)

  (defvar rifle-references-common-path--languages
    (concat org-directory
            "/Wiki/Computer Technology/Programming/Programming Languages/"))

  ;; Emacs modes
  (defun rifle-Emacs-modes-ref ()
    (interactive)
    (let ((my-emacs-modes-reference-dir
           (concat org-directory
                   "/Wiki/Computer Technology/Programming/Emacs/modes/")))
      (helm-org-rifle-directories
       (list my-emacs-modes-reference-dir))))
  
  ;; (define-key reference-prefix (kbd "e") 'rifle-Emacs-modes-ref)

  ;; TeX/LaTeX
  (defun rifle-TeX-ref ()
    (interactive)
    (let ((my-tex-symbols-reference-dir
           (concat rifle-references-common-path--languages
                   "TeX/Data/Manuals/My TeX Symbols Reference/")))
      (helm-org-rifle-directories
       (list my-tex-symbols-reference-dir))))
  
  (define-key reference-prefix (kbd "t") 'rifle-TeX-ref)

  ;; TEST:
  ;; (helm-org-rifle-directories
  ;;  (list (concat rifle-references-common-path--languages
  ;;                "Clojure/Data/Manuals/My Clojure Language Syntax Reference/")))
  
  ;; Clojure
  (defun rifle-Clojure-ref ()
    (interactive)
    (let ((my-clojure-syntax-reference-dir
           (concat rifle-references-common-path--languages
                   "Clojure/Data/Manuals/My Clojure Language Syntax Reference/")))
      (helm-org-rifle-directories
       (list my-clojure-syntax-reference-dir))))
  
  (define-key reference-prefix (kbd "c") 'rifle-Clojure-ref)

  ;; Ruby
  (defun rifle-Ruby-ref ()
    (interactive)
    (let ((my-ruby-syntax-reference-dir
           (concat rifle-references-common-path--languages
                   "Ruby/Data/Manuals/Ruby Language Syntax Reference/")))
      (helm-org-rifle-directories
       (list my-ruby-syntax-reference-dir))))
  
  (define-key reference-prefix (kbd "r") 'rifle-Ruby-ref)

  ;; Python
  (defun rifle-Python-ref ()
    (interactive)
    (let ((my-python-syntax-reference-dir
           (concat rifle-references-common-path--languages
                   "Python/Data/Manuals/My Python Language Syntax Reference/")))
      (helm-org-rifle-directories
       (list my-python-syntax-reference-dir))))
  
  (define-key reference-prefix (kbd "p") 'rifle-Python-ref)
  
  ;; C
  (defun rifle-C-ref ()
    (interactive)
    (let ((my-C-syntax-reference-dir
           (concat rifle-references-common-path--languages
                   "C/Data/Manuals/My C Language Syntax Reference/")))
      (helm-org-rifle-directories
       (list my-C-syntax-reference-dir))))
  
  (define-key reference-prefix (kbd "C") 'rifle-C-ref)
  
  ;; JavaScript
  (defun rifle-JavaScript-ref ()
    (interactive)
    (let ((my-javascript-syntax-reference-dir
           (concat rifle-references-common-path--languages
                   "JavaScript/Data/Manuals/My JavaScript Language Syntax Reference/")))
      (helm-org-rifle-directories
       (list my-javascript-syntax-reference-dir))))
  
  (define-key reference-prefix (kbd "j") 'rifle-JavaScript-ref)

  ;; HTML
  (defun rifle-HTML-ref ()
    (interactive)
    (let ((my-html-syntax-reference-dir
           (concat rifle-references-common-path--languages
                   "HTML/Data/Manuals/My HTML Language Syntax Reference/")))
      (helm-org-rifle-directories
       (list my-html-syntax-reference-dir))))
  
  (define-key reference-prefix (kbd "h") 'rifle-HTML-ref)

  ;; CSS
  (defun rifle-CSS-ref ()
    (interactive)
    (let ((my-css-syntax-reference-dir
           (concat rifle-references-common-path--languages
                   "CSS/Data/Manuals/My CSS Language Syntax Reference/")))
      (helm-org-rifle-directories
       (list my-css-syntax-reference-dir))))
  
  (define-key reference-prefix (kbd "H") 'rifle-CSS-ref)

  ;; Julia
  (defun rifle-Julia-ref ()
    (interactive)
    (let ((my-julia-syntax-reference-dir
           (concat rifle-references-common-path--languages
                   "Julia/Data/Manuals/My Julia Language Syntax Reference/")))
      (helm-org-rifle-directories
       (list my-julia-syntax-reference-dir))))
  
  (define-key reference-prefix (kbd "J") 'rifle-Julia-ref)

  ;; SQL
  (defun rifle-SQL-ref ()
    (interactive)
    (let ((my-sql-syntax-reference-dir
           (concat rifle-references-common-path--languages
                   "Database/SQL/Data/Manuals/My SQL Language Syntax Reference/")))
      (helm-org-rifle-directories
       (list my-sql-syntax-reference-dir))))
  
  (define-key reference-prefix (kbd "s") 'rifle-SQL-ref)

  ;; Commands & Softwares
  (defun rifle-Commands-ref ()
    (interactive)
    (let ((my-commands-reference-dir
           (concat org-directory
                   "/Wiki/Computer Technology/Softwares/")))
      (helm-org-rifle-directories
       (list my-commands-reference-dir))))

  ;; NOTE: this is VERY SLOW!
  ;; (define-key reference-prefix (kbd "M-s") 'rifle-Commands-ref)

  ;; Programming Implements
  (defun rifle-Implements-ref ()
    (interactive)
    (let ((my-implements-reference-dir
           (concat org-directory
                   "/Wiki/Computer Technology/Programming/Implements/")))
      (helm-org-rifle-directories
       (list my-implements-reference-dir))))
  
  (define-key reference-prefix (kbd "i") 'rifle-Implements-ref)

  ;; Computer Technology Glossary
  (defun rifle-Computer-ref ()
    (interactive)
    (let ((my-implements-reference-dir
           (concat org-directory
                   "/Wiki/Computer Technology/Data/Manuals/My Computer Technology Glossary/")))
      (helm-org-rifle-directories
       (list my-implements-reference-dir))))
  
  (define-key reference-prefix (kbd "M-c") 'rifle-Computer-ref)

  ;; Programming Glossary
  (defun rifle-Programming-ref ()
    (interactive)
    (let ((my-implements-reference-dir
           (concat org-directory
                   "/Wiki/Computer Technology/Programming/Data/Manuals/My Programming Glossary/")))
      (helm-org-rifle-directories
       (list my-implements-reference-dir))))
  
  (define-key reference-prefix (kbd "p") 'rifle-Programming-ref)
  
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
       (list my-math-formulas-reference-dir
             my-math-terms-reference-dir))))

  ;; NOTE: this is VERY SLOW!
  ;; (define-key reference-prefix (kbd "m") 'rifle-Math-ref)

  ;; Bookmarks
  (defun rifle-bookmarks-ref ()
    (interactive)
    (let ((my-bookmarks-reference-dir (concat org-directory "/Bookmarks")))
      (helm-org-rifle-directories
       (list my-bookmarks-reference-dir))))

  (define-key reference-prefix (kbd "C-b") 'rifle-bookmarks-ref)
  )

;;; [ helm-fuzzy-find ] -- [C-c C-/], [C-x c /]

(use-package helm-fuzzy-find
  :ensure t
  :after helm
  :ensure-system-package (ff . "cd ~/Code/Emacs/ff/ ; make && mv ff ~/bin/")
  :init
  (defun org-helm-fuzzy-find ()
    (interactive)
    (let ((default-directory org-directory))
	    (when (yes-or-no-p "Heavy recursive on Org root ~/Org/, continue? ")
        (helm-fuzzy-find nil))))
  
  (define-key Org-prefix (kbd "C-f") 'org-helm-fuzzy-find)
  )



(provide 'init-my-org-search)

;;; init-my-org-search.el ends here
