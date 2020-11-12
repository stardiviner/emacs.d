;;; init-org-document-structure.el --- init for Org Document Structure
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(add-hook 'org-mode-hook #'electric-pair-local-mode)

(defun org-mode-electric-pair-setting ()
  (make-local-variable 'electric-pair-pairs)
  (setq-local electric-pair-pairs nil)
  ;; (add-to-list 'electric-pair-pairs '(?\* . ?\*)) ; bold text ; NOTE disable for change headline level.
  ;; (add-to-list 'electric-pair-pairs '(?\/ . ?\/)) ; italic text
  ;; (add-to-list 'electric-pair-pairs '(?\_ . ?\_)) ; underline text
  (add-to-list 'electric-pair-pairs '(?\= . ?\=)) ; verbatim
  (add-to-list 'electric-pair-pairs '(?\~ . ?\~)) ; code
  )
(add-hook 'org-mode-hook #'org-mode-electric-pair-setting)

;; inserting by replace ` with ‘, ' with ’, and replace doubale `` with ”, '' with ”.
;; (add-hook 'org-mode-hook #'electric-quote-local-mode)

(setq org-special-ctrl-a/e t)
(setq org-fontify-whole-heading-line t)
(setq org-fontify-done-headline t)
(setq org-hide-emphasis-markers t)

;;; List

(setq org-hierarchical-todo-statistics nil
      org-checkbox-hierarchical-statistics nil)

;;; it makes sense to have list bullets change with depth.
(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+")))

;; * Column View::

;; (setq org-columns-default-format
;;       "%25ITEM %TODO %3PRIORITY %TAGS %6effort(EFFORT){:}")

;; setup column views for effort estimates
(setq org-columns-default-format
      "%50ITEM(Task) %8TODO %1PRIORITY %14TIMESTAMP_IA(TimeStamp) %Effort(Effort){:} %CLOCKSUM"
      ;; Default column view headings
      ;; - %ITEM(Task) :: Headline (where (Task) is the column head)
      ;; - %PRIORITY :: Priority
      ;; - %TAGS :: tags
      ;; - %ITEMSTAMP_IA :: Timestamp
      ;; - %Effort(Effort){:} :: Effort
      ;; - %CLOCKSUM :: Clock Sum
      )

;; This will be combined with constant `org-global-properties-fixed'
(add-to-list 'org-global-properties '("Effort" . "0:30 0:45 1:00 1:30 2:00"))
(add-to-list 'org-global-properties '("AUTHOR" . "stardiviner"))

;; * Footnotes::                   How footnotes are defined in Org's syntax

(setq org-footnote-auto-label 'confirm
      org-footnote-auto-adjust t
      org-footnote-define-inline nil ; t: define foot inline, instead of separate section.
      org-footnote-section nil ; let footnotes can be under any headline section.
      org-footnote-fill-after-inline-note-extraction t)

;;; [ Structure Templates ] --  ; expand snippet <[s]

;; (require 'org-tempo)
;; (add-to-list 'org-structure-template-alist '("?" . "..."))
;; (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
;; `tempo-define-template'

;;; [ Entities ]

(defun org-insert-entity-with-ivy ()
  "Insert an org-entity using Ivy."
  (interactive)
  (ivy-read "Entity: "
            (loop for element in (append org-entities org-entities-user)
			            when (not (stringp element))
			            collect
			            (cons
			             (format "%10s | %s | %s | %s"
				                   (car element) ;name
				                   (nth 1 element) ; latex
				                   (nth 3 element) ; html
				                   (nth 6 element)) ;utf-8
			             element))
	          :require-match t
            ;; Ivy [M-o]
	          :action '(1
		                  ("u" (lambda (element) (insert (nth 6 (cdr element)))) "utf-8")
		                  ("o" (lambda (element) (insert "\\" (cadr element))) "org-entity")
		                  ("l" (lambda (element) (insert (nth 1 (cdr element)))) "latex")
		                  ("h" (lambda (element) (insert (nth 3 (cdr element)))) "html"))))

;;; [ Table Of Contents (TOC) ]

;;; [ org-make-toc ] -- Automatic tables of contents for Org files.

;; (use-package org-make-toc
;;   :ensure t
;;   :defer t
;;   :commands (org-make-toc))

;;; [ orgtbl-aggregate ] -- create an aggregated Org table from another one.

(use-package orgtbl-aggregate
  :ensure t
  :commands (org-insert-dblock org-insert-dblock:aggregate)
  :config
  ;; add `orgtbl-aggregate' dynamic blocks into list.
  (org-dynamic-block-define "columnview" 'org-insert-dblock:columnview)
  (org-dynamic-block-define "aggregate" 'org-insert-dblock:aggregate)
  (org-dynamic-block-define "invoice" 'org-insert-dblock:invoice)
  (org-dynamic-block-define "join" 'org-insert-dblock:join)
  (org-dynamic-block-define "org-gantt" 'org-insert-dblock:org-gantt)
  (org-dynamic-block-define "propview" 'org-insert-dblock:propview)
  (org-dynamic-block-define "transpose" 'org-insert-dblock:transpose))

;;; [ org-lint ] -- Org-mode linter. [M-x org-lint]

(use-package org-lint
  :commands (org-lint)
  :init (add-to-list 'display-buffer-alist '("^\\*Org Lint\\*" . (display-buffer-below-selected))))


(provide 'init-org-document-structure)

;;; init-org-document-structure.el ends here
