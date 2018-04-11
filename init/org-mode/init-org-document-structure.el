;;; init-org-document-structure.el --- init for Org Document Structure
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; Editing

(setq org-catch-invisible-edits 'smart)

;;; Navigation

(setq org-special-ctrl-a/e t
      org-special-ctrl-o t)

;; * Plain Lists::

(require 'org-list)
(setq org-list-allow-alphabetical t)

;;; [ column view ]

;; (setq org-columns-default-format
;;       "%25ITEM %TODO %3PRIORITY %TAGS %6effort(EFFORT){:}")

;; setup column views for effort estimates
(setq org-columns-default-format
      "%50ITEM(Task) %8TODO %1PRIORITY %14TIMESTAMP_IA(TimeStamp) %Effort(Effort){:}"
      ;; Default column view headings
      ;; - %ITEM(Task) :: Headline (where (Task) is the column head)
      ;; - %PRIORITY :: Priority
      ;; - %TAGS :: tags
      ;; - %CLOCKSUM :: Clock Sum
      ;; - %ITEMSTAMP_IA :: Timestamp
      ;; - %Effort(Effort){:} :: Effort
      
      ;; org-global-properties
      org-agenda-columns-add-appointments-to-effort-sum t
      org-agenda-columns-compute-summary-properties t
      )

;;; Statistics [1/4]

(setq org-hierarchical-todo-statistics nil
      org-checkbox-hierarchical-statistics nil)

;; * Table

;; * Plotting

;; [ org-plot ] -- Plotting Tables in Org-mode.

(require 'org-plot)

;;; Org Table translator functions.
(add-to-list 'org-default-properties "ORGTBL") ; for Org-mode Table translator functions.

;; define a keybinding for org table translator functions
(define-key org-mode-map (kbd "C-c \" i") 'orgtbl-insert-radio-table)
(define-key org-mode-map (kbd "C-c \" s") 'orgtbl-send-table)

;;; TODO: does this work?
;; will be combined with constant `org-global-properties-fixed'
;; (add-to-list 'org-global-properties '("Effort" . "0:30"))
;; (add-to-list 'org-global-properties '("Title" . nil))
;; (add-to-list 'org-global-properties '("Author" . "stardiviner"))

;; * Blocks::                      Folding blocks

;; * Footnotes::                   How footnotes are defined in Org's syntax

(setq org-footnote-auto-label 'confirm
      org-footnote-auto-adjust t
      org-footnote-define-inline nil ; t: define foot inline, instead of separate section.
      org-footnote-fill-after-inline-note-extraction t
      )

;;; [ Structure Templates ] -- <[s] expand.

;; (add-to-list 'org-structure-template-alist '("?" . "..."))


;;; [ Entities ]

(defun org-insert-entity-with-ivy ()
  "Insert an org-entity using Ivy."
  (interactive)
  (ivy-read "Entity: " (loop for element in (append org-entities org-entities-user)
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
	          :action '(1
		                  ("u" (lambda (element) (insert (nth 6 (cdr element)))) "utf-8")
		                  ("o" (lambda (element) (insert "\\" (cadr element))) "org-entity")
		                  ("l" (lambda (element) (insert (nth 1 (cdr element)))) "latex")
		                  ("h" (lambda (element) (insert (nth 3 (cdr element)))) "html"))))

;;; [ Dynamic Blocks ] -- [C-c C-x i]


(provide 'init-org-document-structure)

;;; init-org-document-structure.el ends here
