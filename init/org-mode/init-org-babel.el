;;; init-org-babel.el --- init for Org Babel
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(setq org-confirm-babel-evaluate nil)
(setq org-babel-hash-show-time t) ; header argument: :cache yes.

;;; add org-babel header-args property into default properties list.
(add-to-list 'org-default-properties "header-args")

;;; source block default header arguments
(add-to-list 'org-babel-default-header-args '(:cache . "yes"))
(add-to-list 'org-babel-default-header-args '(:comments . "links"))

;;; Manage org-babel popup buffers with `display-buffer-alist'.
(add-to-list 'display-buffer-alist
             '("^\\*Org- Src.*\\*" (display-buffer-below-selected)))
(add-to-list 'display-buffer-alist
             '("^\\*Org-Babel Preview.*\\*" (display-buffer-below-selected)))
(add-to-list 'display-buffer-alist
             '("^\\*Org-Babel Error Output\\*" (display-buffer-below-selected)))

;;; [ inline call code block ]

;; (setq org-babel-exp-inline-code-template "src_%lang[%switches%flags]{%body}")
;; (setq org-babel-default-inline-header-args)
;; or "=%s=", "~%s~"
;; (setq org-babel-inline-result-wrap "=> (~%s~)")

;;; [ noweb ]
;; Raise errors when noweb references don't resolve.
(setq org-babel-noweb-error-all-langs t)

;; babel src block editing
(setq org-src-fontify-natively t
      ;; nil: preserve org indent, t: preserve export indent.
      org-src-preserve-indentation nil
      ;; 0: fix `diff' babel syntax highlighting invalid issue.
      org-edit-src-content-indentation 0
      org-src-window-setup 'current-window ; 'reorganize-frame, 'current-window
      org-src-ask-before-returning-to-edit-buffer nil
      )

(setq org-babel-load-languages
      '((org . t)                            ; Org-mode
        (shell . t)                          ; Shell Script
        (calc . t)                           ; Calc
        ))

;;; [ ob-shell ]

(require 'ob-shell)
(add-to-list 'org-babel-load-languages '(shell . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(add-to-list 'org-babel-tangle-lang-exts '("shell" . "sh"))

(add-to-list 'org-babel-default-header-args:shell
             '(:results . "output"))
(add-to-list 'org-babel-default-header-args:shell
             '(:noweb . "yes"))

;;; [ Tangle ]

(setq org-babel-tangle-lang-exts
      '(("latex" . "tex")
        ("awk" . "awk")))

(setq org-babel-tangle-use-relative-file-links t
      ;; org-babel-pre-tangle-hook '(save-buffer)
      ;; org-babel-post-tangle-hook
      )


;;; [ Library of Babel ]

;;; automatically ingest "Library of Babel".
(org-babel-lob-ingest
 (concat user-emacs-directory "Org-mode/Library of Babel/Library of Babel.org"))

;; (setq org-babel-default-lob-header-args)

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
– Report the line number instead of the char position."
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

;;; Tangling with append to file instead of default overwrite.
(defun org-babel-tangle-append ()
  "Append source code block at point to its tangle file.
The command works like `org-babel-tangle' with prefix arg
but `delete-file' is ignored."
  (interactive)
  (cl-letf (((symbol-function 'delete-file) #'ignore))
    (org-babel-tangle '(4))))

(org-defkey org-mode-map (kbd "C-c C-v M-t") 'org-babel-tangle-append)

;;; [ ob-async ] -- enables asynchronous execution of org-babel src blocks for *any* languages.

(use-package ob-async
  :ensure t)

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

;;; [ helm-lib-babel ] -- Emacs helm extension for inserting a reference to an org source block function.

(use-package helm-lib-babel
  :ensure t
  :bind (:map org-babel-map ("M-i" . helm-lib-babel-insert)))

;; load all languages at last.
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)



(provide 'init-org-babel)

;;; init-org-babel.el ends here
