;;; init-org-babel.el --- init for Org Babel
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(setq org-confirm-babel-evaluate nil)
(setq org-babel-hash-show-time t) ; header argument: :cache yes.

(add-to-list 'display-buffer-alist
             '("^\\*Org-Babel Results\\*" . (display-buffer-reuse-window display-buffer-below-selected)))

;;; add org-babel header-args property into default properties list.
(add-to-list 'org-default-properties "header-args")

;;; source block default header arguments
(setq-default org-babel-default-header-args
              '((:session . "none")
                (:noweb . "no") (:hlines . "no")
                (:tangle . "no") (:comments . "links")
                (:cache . "yes")
                (:results . "replace")
                ;; for exporting
                (:eval . "never-export")
                ;; (:exports . "both") conflict with (:eval "never-export")
                ))

;;; don't evaluate babel when exporting. For helper functions like `my:org-convert-region-to-html' etc.
;; (setq org-export-babel-evaluate nil)

;;; Manage org-babel popup buffers with `display-buffer-alist'.
(add-to-list 'display-buffer-alist
             '("^\\*Org- Src.*\\*" (display-buffer-below-selected)))
(add-to-list 'display-buffer-alist
             '("^\\*Org-Babel Preview.*\\*" (display-buffer-below-selected)))
(add-to-list 'display-buffer-alist
             '("^\\*Org-Babel Error Output\\*" (display-buffer-below-selected)))

;;; [ inline call code block ]

;; (setq org-babel-exp-inline-code-template "src_%lang[%switches%flags]{%body}")
(setq org-babel-default-inline-header-args
      '((:session . "none")
        (:results . "replace")
        (:exports . "both")
        (:hlines . "yes")))
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
      org-src-window-setup 'current-window ; 'split-window-below
      org-src-ask-before-returning-to-edit-buffer nil
      )

(setq org-babel-load-languages
      '((org . t)                       ; Org-mode
        ;; (shell . t)                     ; Shell Script
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

(defvar org-babel-default-header-args:sh '())
(add-to-list 'org-babel-default-header-args:sh
             '(:results . "output"))

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
(defun my/org-babel-insert-named-src-block (&optional template)
  (interactive)
  (let ((src-block
	       (completing-read "Enter src block name[or TAB or ENTER]: " (org-babel-src-block-names))))
    (unless (string-equal "" src-block)
	    (insert (format "<<%s>>" src-block)))))
(define-key org-babel-map (kbd "M-q") 'my/org-babel-insert-named-src-block)

(defun my/org-babel-insert-src-block-call (&optional template)
  "Interactively insert a named src block call with `TEMPLATE'."
  (interactive)
  (let ((template (or template "#+call: %s()\n"))
	      (src-block (completing-read "Enter src block name[or TAB or ENTER]: " (org-babel-src-block-names))))
    (unless (string-equal "" src-block)
	    (insert (format template src-block)))))
(define-key org-babel-map (kbd "M-c") 'my/org-babel-insert-src-block-call)

;;; [ Literate dotfiles management with Org-mode ]

;; Tangle Org files when we save them
;; (defun tangle-on-save-org-mode-file()
;;   (when (string= (message "%s" major-mode) "org-mode")
;;     (org-babel-tangle)))
;; (add-hook 'after-save-hook 'tangle-on-save-org-mode-file)

;;; [ coderef ]
;;; prepend comment char ahead of `org-coderef-label'.
;; auto prefix with comment char when create code ref in src block with `org-store-link'.
(use-package smartparens
  :ensure t
  :defer t
  :init
  (defun my-org-src-coderef-format (result)
    "Auto prefix with comment char before `org-coderef-label' `RESULT'."
    (if sp-comment-char
        (format "%s %s" sp-comment-char result)
      result))
  (advice-add 'org-src-coderef-format :filter-return 'my-org-src-coderef-format))


(add-hook 'org-src-mode-hook #'sound-tick)

(add-hook 'org-babel-after-execute-hook #'sound-voice-complete)
(add-hook 'org-babel-post-tangle-hook #'sound-voice-complete)
(add-hook 'org-babel-pre-tangle-hook #'sound-tick)

;;; Tangling with append to file instead of default overwrite.
(defun my/org-babel-tangle-append ()
  "Append source code block at point to its tangle file.
The command works like `org-babel-tangle' with prefix arg
but `delete-file' is ignored."
  (interactive)
  (cl-letf (((symbol-function 'delete-file) #'ignore))
    (org-babel-tangle '(4))))
(define-key org-babel-map (kbd "M-t") 'my/org-babel-tangle-append)

;;; [ ob-async ] -- enables asynchronous execution of org-babel src blocks for *any* languages.

(use-package ob-async
  :ensure t
  :defer t)

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
  :defer t
  :bind (:map org-babel-map ("M-i" . helm-lib-babel-insert)))

;;; [ org-radiobutton ] -- Get the checked item from a check list to be used for
;;; Org-mode Literate Programming variable.

;; (use-package org-radiobutton
;;   :ensure t
;;   :defer t
;;   :init (global-org-radiobutton-mode))

;; load all languages at last.
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)



(provide 'init-org-babel)

;;; init-org-babel.el ends here
